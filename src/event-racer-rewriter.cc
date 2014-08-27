#include "src/compiler.h"
#include "src/event-racer-rewriter.h"
#include "src/scopes.h"

namespace v8 {

namespace internal {

EventRacerRewriter::AstRewriterImpl(CompilationInfo *info)
  : info_(info), post_scope_analysis_(false),
    current_context_(NULL),
    factory_(info_->zone(), info_->ast_value_factory()),
    arg_names_(NULL) {

  InitializeAstRewriter(info->zone());

  Scope &globals = *info->global_scope();
  AstValueFactory &values = *info->ast_value_factory();

  Variable *v;
  v = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_read"));
  ER_read_proxy_ = factory_.NewVariableProxy(v);
  v = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_readProp"));
  ER_readProp_proxy_ = factory_.NewVariableProxy(v);
  o_string_ = values.GetOneByteString("$obj");
  k_string_ = values.GetOneByteString("$key");
}

Scope *EventRacerRewriter::NewScope(Scope* outer, ScopeType type) {
  
  Scope* s = new (zone()) Scope(outer ? outer : info_->global_scope(),
                                type, info_->ast_value_factory(),
                                zone());
  s->Initialize();
  return s;
}

void EventRacerRewriter::ensure_arg_names(int n) {
  if (arg_names_ == NULL)
    arg_names_ = new (zone()) ZoneList<const AstRawString *>(n, zone());
  EmbeddedVector<char, 16> buf;
  for(int i = arg_names_->length(); i < n; ++i) {
    SNPrintF(buf, "$a%d", i);
    arg_names_->Add(info_->ast_value_factory()->GetOneByteString(buf.start()),
                    zone());
  }
}

template<typename T> void rewrite(AstRewriter *w, T *&node) {
  if (node)
    node = node->Accept(w);
};

template<typename T> void rewrite(AstRewriter *w, ZoneList<T *> *lst) {
  if (lst) {
    const int n = lst->length();
    for (int i = 0; i < n; ++i)
      lst->at(i) = lst->at(i)->Accept(w);
  }
}

Block* EventRacerRewriter::doVisit(Block *blk) {
  ContextScope _(this, blk->scope());
  rewrite(this, blk->statements());
  return blk;
}

ExpressionStatement *EventRacerRewriter::doVisit(ExpressionStatement *st) {
  rewrite(this, st->expression_);
  return st;
}

DoWhileStatement *EventRacerRewriter::doVisit(DoWhileStatement *st) {
  rewrite(this, st->cond_);
  rewrite(this, st->body_);
  return st;
}

WhileStatement *EventRacerRewriter::doVisit(WhileStatement *st) {
  rewrite(this, st->cond_);
  rewrite(this, st->body_);
  return st;
}

ForStatement *EventRacerRewriter::doVisit(ForStatement *st) {
  rewrite(this, st->init_);
  rewrite(this, st->cond_);
  rewrite(this, st->next_);
  rewrite(this, st->body_);
  return st;
}

ForInStatement *EventRacerRewriter::doVisit(ForInStatement *st) {
  // TODO rewrite(this, st->each_);
  rewrite(this, st->subject_);
  rewrite(this, st->body_);
  return st;
}

ForOfStatement *EventRacerRewriter::doVisit(ForOfStatement *st) {
  // TODO rewrite(this, st->each_);
  rewrite(this, st->subject_);
  rewrite(this, st->body_);
  rewrite(this, st->assign_iterator_);
  rewrite(this, st->next_result_);
  rewrite(this, st->result_done_);
  rewrite(this, st->assign_each_);
  return st;
}

ReturnStatement *EventRacerRewriter::doVisit(ReturnStatement *st) {
  rewrite(this, st->expression_);
  return st;
}

WithStatement *EventRacerRewriter::doVisit(WithStatement *st) {
  ContextScope _(this, st->scope());
  rewrite(this, st->expression_);
  rewrite(this, st->statement_);
  return st;
}

CaseClause *EventRacerRewriter::doVisit(CaseClause *ex) {
  rewrite(this, ex->label_);
  rewrite(this, ex->statements());
  return ex;
}

SwitchStatement *EventRacerRewriter::doVisit(SwitchStatement *st) {
  rewrite(this, st->tag_);
  rewrite(this, st->cases());
  return st;
}

IfStatement *EventRacerRewriter::doVisit(IfStatement *st) {
  rewrite(this, st->condition_);
  rewrite(this, st->then_statement_);
  rewrite(this, st->else_statement_);
  return st;
}

TryCatchStatement *EventRacerRewriter::doVisit(TryCatchStatement *st) {
  rewrite(this, st->try_block_);
  ContextScope _(this, st->scope());
  rewrite(this, st->catch_block_);
  return st;
}

TryFinallyStatement *EventRacerRewriter::doVisit(TryFinallyStatement *st) {
  rewrite(this, st->try_block_);
  rewrite(this, st->finally_block_);
  return st;
}

ObjectLiteral* EventRacerRewriter::doVisit(ObjectLiteral *lit) {
  if (lit->properties()) {
    ZoneList<ObjectLiteral::Property *> &ps = *lit->properties();
    for (int i = 0; i < ps.length(); ++i)
      rewrite(this, ps[i]->value_);
  }
  return lit;
}

ArrayLiteral* EventRacerRewriter::doVisit(ArrayLiteral *lit) {
  rewrite(this, lit->values());
  return lit;
}

Expression* EventRacerRewriter::doVisit(VariableProxy *vp) {
  // Postpone the rewriting of variable proxies for after the scope
  // analysis and variable resolution have ran.
  if (!post_scope_analysis_) {
    if (vp->var() == NULL)
      // If the variable proxy is not yet bound to a variable, record it
      // as unresolved in the current scope. Note that we may introduce
      // scopes, for which the parser haven't had the chance to record
      // the names, which need resolutuion.
      vp = context()->scope->NewUnresolved(&factory_,
                                           vp->raw_name(),
                                           Interface::NewValue(),
                                           vp->position());
    return vp;
  }

  // Instrument only access to potentially shared variables, namely
  // those declared by the user (as opposed to being introduced by the
  // compiler), which aren't stack allocated.
  if (!is_potentially_shared(vp))
    return vp;

  // Read of a property of the global object is rewritten into a call to
  // ER_read: |g => ER_read("g", g)|
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(2, zone());
  args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()), zone());
  args->Add(vp, zone());
  return factory_.NewCall(ER_read_proxy_, args, vp->position());
}


Expression* EventRacerRewriter::doVisit(Property *p) {

  // Property read is rewritten pre scope analysis.
  if (post_scope_analysis_)
    return p;

  // Read of a property of an object is rewritten into a call to
  // ER_readProp.

  // If the key is a literal, the Property is rewritten into a call
  // like:

  // |obj.key|
  //  => |(function(o) { return ER_readProp(p, "key", o.key); })(obj)|
  
  // If the key as a general expression, the Property is rewritten into
  // a call like:
 
  // |obj.key|
  //  => |(function(o, k) { return ER_readProp(o, k, o[k]); })(obj, key);|

  // The function literal is needed in order to ensure |obj| and |key|
  // are evaluated exactly once. Passing the expressions as parameters
  // avoids capturing variabes in the inner closure, which may cause
  // them to be context allocated.

  Expression *obj = p->obj_, *key = p->key_;

  Scope *scope = NewScope(context()->scope, FUNCTION_SCOPE);
  scope->set_start_position(p->position());
  scope->set_end_position(p->position() + 1);

  // Declare parameters of the new function.
  scope->DeclareParameter(o_string_, VAR);
  if (!key->IsLiteral())
    scope->DeclareParameter(k_string_, VAR);

  // Setup arguments of the call to |ER_readProp|.
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(3, zone());

  // First argument is the |$obj| parameter.
  VariableProxy *o_proxy = 
    scope->NewUnresolved(&factory_, o_string_, Interface::NewValue(),
                         p->position());
  args->Add(o_proxy, zone());

  // Second argument is either the |$key| parameter or, if the
  // property name is a literal, the literal itself.
  if (key->IsLiteral())
    args->Add(key, zone());
  else {
    VariableProxy *k_proxy;
    k_proxy = scope->NewUnresolved(&factory_, k_string_, Interface::NewValue(),
                                   p->position());
    args->Add(k_proxy, zone());
    p->key_ = k_proxy;
  }
  p->obj_ = o_proxy;

  // Third parameter is the property expression.
  args->Add(p, zone());
  Call *ret = factory_.NewCall(ER_readProp_proxy_, args, p->position());

  // Define the new function.
  ZoneList<Statement*> *body = new (zone()) ZoneList<Statement*>(1, zone());

  FunctionLiteral* fn = factory_.NewFunctionLiteral(
    info_->ast_value_factory()->empty_string(),
    info_->ast_value_factory(),
    scope,
    body,
    /* materialized_literal_count */ 0,
    /* expected_property_count */ 0,
    /* handler_count */ 0,
    /* num_parameters */ 1 + !key->IsLiteral(),
    FunctionLiteral::kNoDuplicateParameters,
    FunctionLiteral::ANONYMOUS_EXPRESSION,
    FunctionLiteral::kIsFunction,
    FunctionLiteral::kIsParenthesized,
    FunctionLiteral::kNormalFunction,
    p->position());
  body->Add(factory_.NewReturnStatement(ret, p->position()), zone());

  // Build the call to the new function.
  args = new (zone()) ZoneList<Expression*>(2, zone());
  args->Add(obj, zone());
  if (!key->IsLiteral())
    args->Add(key, zone());

  return factory_.NewCall(fn, args, p->position());
}

Call* EventRacerRewriter::doVisit(Call *c) {
  // Calls are rewritten pre scope analysis and non-property-calls
  // aren't treated specially.
  if (post_scope_analysis_ || !c->expression_->IsProperty()) {
    rewrite(this, c->expression_);
    rewrite(this, c->arguments());
    return c;
  }

  // Call of a property is instrumented like:
  // o.f(e0, e2, ..., en)
  // =>
  // (function($a0, $a1, ..., $an, $obj, $key) {
  //    ER_readProp($obj, $key, $obj.$key);
  //    return $obj.$key($a0, $a1, ..., $an); })
  //   (e0, e1, en, ..., o, f)
  // Property calls are treated by the compiler in a special manner -
  // the callee gets a receiver object - hence we should end up with an
  // instrumented expression, which also contains an analoguos property
  // call.

  Property *p = c->expression_->AsProperty();
  Expression *obj = p->obj_, *key = p->key_;

  Scope *scope = NewScope(context()->scope, FUNCTION_SCOPE);
  scope->set_start_position(c->position());
  scope->set_end_position(c->position() + 1);

  // Declare parameters of the new function.
  const int n = c->arguments()->length();
  ensure_arg_names(n);
  for (int i = 0; i < n; ++i)
    scope->DeclareParameter(arg_names_->at(i), VAR);
  scope->DeclareParameter(o_string_, VAR);
  if (!key->IsLiteral())
    scope->DeclareParameter(k_string_, VAR);

  // Setup arguments of the call to |ER_readProp|.
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(3, zone());

  // First argument is the |$obj| parameter.
  VariableProxy *o_proxy = 
    scope->NewUnresolved(&factory_, o_string_, Interface::NewValue(),
                         c->position());
  args->Add(o_proxy, zone());

  // Second argument is either the |$key| parameter or, if the
  // property name is a literal, the literal itself.
  if (key->IsLiteral())
    args->Add(key, zone());
  else {
    VariableProxy *k_proxy;
    k_proxy = scope->NewUnresolved(&factory_, k_string_, Interface::NewValue(),
                                   c->position());
    args->Add(k_proxy, zone());
    p->key_ = k_proxy;
  }
  p->obj_ = o_proxy;

  // Third parameter is the property expression.
  args->Add(p, zone());
  Statement *er_call = factory_.NewExpressionStatement(
    factory_.NewCall(ER_readProp_proxy_, args, c->position()),
    c->position());

  // Build the inner property call.
  args = new (zone()) ZoneList<Expression *>(n, zone());
  for (int i = 0; i < n; ++i) {
    args->Add(scope->NewUnresolved(&factory_, arg_names_->at(i),
                                   Interface::NewValue(), c->position()),
              zone());
  }

  Statement *ret = factory_.NewReturnStatement(
    factory_.NewCall(factory_.NewProperty(p->obj_, p->key_, p->position()),
                     args, c->position()),
    c->position());

  // Define the new function.
  ZoneList<Statement*> *body = new (zone()) ZoneList<Statement*>(1, zone());

  FunctionLiteral* fn = factory_.NewFunctionLiteral(
    info_->ast_value_factory()->empty_string(),
    info_->ast_value_factory(),
    scope,
    body,
    /* materialized_literal_count */ 0,
    /* expected_property_count */ 0,
    /* handler_count */ 0,
    /* num_parameters */ n + 1 + !key->IsLiteral(),
    FunctionLiteral::kNoDuplicateParameters,
    FunctionLiteral::ANONYMOUS_EXPRESSION,
    FunctionLiteral::kIsFunction,
    FunctionLiteral::kIsParenthesized,
    FunctionLiteral::kNormalFunction,
    c->position());
  body->Add(er_call, zone());
  body->Add(ret, zone());

  // Build the call to the new function.
  args = new (zone()) ZoneList<Expression*>(n + 2, zone());
  for (int i = 0; i < n; ++i)
    args->Add(c->arguments()->at(i), zone());
  args->Add(obj, zone());
  if (!key->IsLiteral())
    args->Add(key, zone());

  return factory_.NewCall(fn, args, p->position());
}

CallNew* EventRacerRewriter::doVisit(CallNew *c) {
  rewrite(this, c->expression_);
  rewrite(this, c->arguments());
  return c;
}

CallRuntime* EventRacerRewriter::doVisit(CallRuntime *c) {
  rewrite(this, c->arguments());
  return c;
}

UnaryOperation* EventRacerRewriter::doVisit(UnaryOperation *op) {
  rewrite(this, op->expression_);
  return op;
}

BinaryOperation* EventRacerRewriter::doVisit(BinaryOperation *op) {
  rewrite(this, op->left_);
  rewrite(this, op->right_);
  return op;
}

CountOperation* EventRacerRewriter::doVisit(CountOperation *op) {
  // TODO: rewrite(this, op->expression_);
  return op;
}

CompareOperation* EventRacerRewriter::doVisit(CompareOperation *op) {
  rewrite(this, op->left_);
  rewrite(this, op->right_);
  return op;
}

Conditional* EventRacerRewriter::doVisit(Conditional *op) {
  rewrite(this, op->condition_);
  rewrite(this, op->then_expression_);
  rewrite(this, op->else_expression_);
  return op;
}

Assignment* EventRacerRewriter::doVisit(Assignment *op) {
  // TODO:  rewrite(this, op->target_);
  rewrite(this, op->value_);
  return op;
}

Yield* EventRacerRewriter::doVisit(Yield *op) {
  if (op->yield_kind() == Yield::SUSPEND || op->yield_kind() == Yield::FINAL)
    rewrite(this, op->expression_);
  return op;
}

Throw* EventRacerRewriter::doVisit(Throw *op) {
  rewrite(this, op->exception_);
  return op;
}

FunctionLiteral* EventRacerRewriter::doVisit(FunctionLiteral *lit) {
  ContextScope _(this, lit->scope());
  rewrite(this, lit->body());
  return lit;
}


// -----------------------------------------------------------------------------

void AstSlotCounter::add_node() {
  ++state_->node_count;
}

void AstSlotCounter::add_materialized_literal(MaterializedLiteral *lit) {
  lit->set_literal_index(state_->materialized_literal_count++);
}

void AstSlotCounter::add_feedback_slot(FeedbackSlotInterface *nd) {
  int cnt = nd->ComputeFeedbackSlotCount();
  if (cnt > 0) {
    nd->SetFirstFeedbackSlot(state_->feedback_slot_count);
    state_->feedback_slot_count += cnt;
  }
}

template<typename T> void traverse(AstVisitor *v, T *node) {
  if (node)
    node->Accept(v);
}

template<typename T> void traverse(AstVisitor *v, ZoneList<T *> *lst) {
  if (lst) {
    const int n = lst->length();
    for (int i = 0; i < n; ++i)
      lst->at(i)->Accept(v);
  }
}

#define AST_LEAF_NODE_LIST(V)                   \
  V(ModuleUrl)                                  \
  V(DebuggerStatement)                          \
  V(ContinueStatement)                          \
  V(BreakStatement)                             \
  V(EmptyStatement)                             \
  V(Literal)                                    \
  V(NativeFunctionLiteral)                      \
  V(ThisFunction)

#define LEAF_VISIT(type)                                \
    void AstSlotCounter::Visit##type(type *nd) {        \
      add_node();                                       \
    }
  AST_LEAF_NODE_LIST(LEAF_VISIT)
#undef LEAF_VISIT
    
void AstSlotCounter::VisitVariableDeclaration(VariableDeclaration *dcl) {
  add_node();
  traverse(this, dcl->proxy());
}

void AstSlotCounter::VisitFunctionDeclaration(FunctionDeclaration *dcl) {
  add_node();
  traverse(this, dcl->proxy());
  traverse(this, dcl->fun());
}

void AstSlotCounter::VisitModuleDeclaration(ModuleDeclaration *dcl) {
  add_node();
  traverse(this, dcl->proxy());
  traverse(this, dcl->module());
}

void AstSlotCounter::VisitImportDeclaration(ImportDeclaration *dcl) {
  add_node();
  traverse(this, dcl->proxy());
  // FIXME: traverse(this, dcl->module());
}

void AstSlotCounter::VisitExportDeclaration(ExportDeclaration *dcl) {
  add_node();
  traverse(this, dcl->proxy());
}

void AstSlotCounter::VisitModuleLiteral(ModuleLiteral *mod) {
  add_node();
  traverse(this, mod->body());
}

void AstSlotCounter::VisitModuleVariable(ModuleVariable *mod) {
  add_node();
  traverse(this, mod->proxy());
}

void AstSlotCounter::VisitModulePath(ModulePath *path) {
  add_node();
}

void AstSlotCounter::VisitModuleStatement(ModuleStatement *st) {
  add_node();
  traverse(this, st->proxy());
  traverse(this, st->body());
}

void AstSlotCounter::VisitBlock(Block *blk) {
  add_node();
  traverse(this, blk->statements());
}

void AstSlotCounter::VisitExpressionStatement(ExpressionStatement *st) {
  add_node();
  traverse(this, st->expression());
}

void AstSlotCounter::VisitDoWhileStatement(DoWhileStatement *st) {
  add_node();
  traverse(this, st->cond());
  traverse(this, st->body());
}

void AstSlotCounter::VisitWhileStatement(WhileStatement *st) {
  add_node();
  traverse(this, st->cond());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForStatement(ForStatement *st) {
  add_node();
  traverse(this, st->init());
  traverse(this, st->cond());
  traverse(this, st->next());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForInStatement(ForInStatement *st) {
  add_node();
  add_feedback_slot(st);
  traverse(this, st->each());
  traverse(this, st->subject());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForOfStatement(ForOfStatement *st) {
  add_node();
  traverse(this, st->each());
  traverse(this, st->subject());
  traverse(this, st->body());
  traverse(this, st->assign_iterator());
  traverse(this, st->next_result());
  traverse(this, st->result_done());
  traverse(this, st->assign_each());
}

void AstSlotCounter::VisitReturnStatement(ReturnStatement *st) {
  add_node();
  traverse(this, st->expression());
}

void AstSlotCounter::VisitWithStatement(WithStatement *st) {
  add_node();
  traverse(this, st->expression());
  traverse(this, st->statement());
}

void AstSlotCounter::VisitCaseClause(CaseClause *ex) {
  add_node();
  if (!ex->is_default())
    traverse(this, ex->label());
  traverse(this, ex->statements());
}

void AstSlotCounter::VisitSwitchStatement(SwitchStatement *st) {
  add_node();
  traverse(this, st->tag());
  traverse(this, st->cases());
}

void AstSlotCounter::VisitIfStatement(IfStatement *st) {
  add_node();
  traverse(this, st->condition());
  traverse(this, st->then_statement());
  traverse(this, st->else_statement());
}

void AstSlotCounter::VisitTryCatchStatement(TryCatchStatement *st) {
  add_node();
  traverse(this, st->try_block());
  traverse(this, st->catch_block());
}

void AstSlotCounter::VisitTryFinallyStatement(TryFinallyStatement *st) {
  add_node();
  traverse(this, st->try_block());
  traverse(this, st->finally_block());
}

void AstSlotCounter::VisitObjectLiteral(ObjectLiteral *lit) {
  add_node();
  add_materialized_literal(lit);
  if (lit->properties()) {
    ZoneList<ObjectLiteral::Property *> &ps = *lit->properties();
    for (int i = 0; i < ps.length(); ++i)
      traverse(this, ps[i]->value());
  }
}

void AstSlotCounter::VisitArrayLiteral(ArrayLiteral *lit) {
  add_node();
  add_materialized_literal(lit);
  traverse(this, lit->values());
}

void AstSlotCounter::VisitVariableProxy(VariableProxy *vp) {
  add_node();
  // We may encounter variables created with the AstNullVisitor,
  // therefore without a feedback slot assigned.
  if (vp->VariableFeedbackSlot() != FeedbackSlotInterface::kInvalidFeedbackSlot)
    add_feedback_slot(vp);
}

void AstSlotCounter::VisitProperty(Property *p) {
  add_node();
  add_feedback_slot(p);
  traverse(this, p->obj());
  traverse(this, p->key());
}

void AstSlotCounter::VisitCall(Call *c) {
  add_node();
  add_feedback_slot(c);
  traverse(this, c->expression());    
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitCallNew(CallNew *c) {
  add_node();
  add_feedback_slot(c);
  traverse(this, c->expression());
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitCallRuntime(CallRuntime *c) {
  add_node();
  add_feedback_slot(c);
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitUnaryOperation(UnaryOperation *op) {
  add_node();
  traverse(this, op->expression());
}

void AstSlotCounter::VisitBinaryOperation(BinaryOperation *op) {
  add_node();
  traverse(this, op->left());
  traverse(this, op->right());
}

void AstSlotCounter::VisitCountOperation(CountOperation *op) {
  add_node();
  traverse(this, op->expression());
}

void AstSlotCounter::VisitCompareOperation(CompareOperation *op) {
  add_node();
  traverse(this, op->left());
  traverse(this, op->right());
}

void AstSlotCounter::VisitConditional(Conditional *op) {
  add_node();
  traverse(this, op->condition());
  traverse(this, op->then_expression());
  traverse(this, op->else_expression());
}

void AstSlotCounter::VisitAssignment(Assignment *op) {
  add_node();
  traverse(this, op->target());
  traverse(this, op->value());
}

void AstSlotCounter::VisitYield(Yield *op) {
  // FIXME:  if (op->yield_kind() == Yield::SUSPEND || op->yield_kind() == Yield::FINAL)
  add_node();
  add_feedback_slot(op);
  traverse(this, op->expression());
}

void AstSlotCounter::VisitThrow(Throw *op) {
  add_node();
  traverse(this, op->exception());
}

void AstSlotCounter::VisitRegExpLiteral(RegExpLiteral *lit) {
  add_node();
  add_materialized_literal(lit);
}

void AstSlotCounter::VisitFunctionLiteral(FunctionLiteral *fn) {
  FunctionState state;
  begin_function(&state);
  add_node();
  Scope *scope = fn->scope();
  if (scope->is_function_scope())
    traverse(this, scope->function());
  traverse(this, scope->declarations());
  traverse(this, fn->body());
  fn->set_materialized_literal_count(state.materialized_literal_count - JSFunction::kLiteralsPrefixSize);

  AstProperties props;
  *props.flags() = *fn->flags();
  props.add_node_count(fn->ast_node_count());
  props.increase_feedback_slots(state.feedback_slot_count);
  fn->set_ast_properties(&props);

  end_function();
}

} } // namespace v8::internal
