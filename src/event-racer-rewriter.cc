#include "src/compiler.h"
#include "src/event-racer-rewriter.h"
#include "src/scopes.h"

namespace v8 {

namespace internal {

EventRacerRewriter::AstRewriterImpl(CompilationInfo *info)
  : info_(info), post_scope_analysis_(false),
    current_context_(NULL),
    factory_(info_->zone(), info_->ast_value_factory()) {

  InitializeAstRewriter(info->zone());

  Scope &globals = *info->global_scope();
  AstValueFactory &values = *info->ast_value_factory();

  Variable *v;
  v = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_read"));
  ER_read_proxy_ = factory_.NewVariableProxy(v);
  v = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_readProp"));
  ER_readProp_proxy_ = factory_.NewVariableProxy(v);
}

Scope *EventRacerRewriter::NewScope(Scope* outer, ScopeType type) {
  
  Scope* s = new (zone()) Scope(outer ? outer : info_->global_scope(),
                                type, info_->ast_value_factory(),
                                zone());
  s->Initialize();
  return s;
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
  rewrite(this, st->assign_iterable_);
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

  // Instrument only access to variables, which are properties of the
  // global (or the window) object.
  Variable *v = vp->var();
  if (!v || v->mode() != VariableMode::VAR)
    return vp;

  // Read of a property of the global object is rewritten into a call to
  // ER_read: |g => ER_read("g", g)|
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(2, zone());
  args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()), zone());
  args->Add(vp, zone());
  return factory_.NewCall(ER_read_proxy_, args, vp->position());
}

Expression* EventRacerRewriter::doVisit(Property *p) {
  // Read of a property of an object is rewritten into a call to
  // ER_readProp:

  // |obj.key| =>
  // |function() {let o = obj, k = key; return ER_readProp(o, k, o.k); }()|

  // The function literal is needed in order to ensure |obj| and |key|
  // are evaluated exactly once.

  // Property read is rewritten pre scope analysis.
  if (post_scope_analysis_) {
    return p;
  }

  Scope *scope = NewScope(context()->scope, ScopeType::FUNCTION_SCOPE);
  scope->set_start_position(p->position());
  scope->set_end_position(p->position() + 1);

  Block *init = factory_.NewBlock(NULL, 2, true, p->position());

  VariableProxy *o_proxy = 
    scope->NewUnresolved(&factory_,
                         info_->ast_value_factory()->GetOneByteString("$o"),
                         Interface::NewValue(),
                         p->position());
  Declaration *decl =
    factory_.NewVariableDeclaration(o_proxy, VariableMode::LET, scope,
                                    p->position());
  Variable *var = scope->DeclareLocal(o_proxy->raw_name(), decl->mode(),
                                      decl->initialization(),
                                      o_proxy->interface());
  var->set_initializer_position(p->position());
  o_proxy->BindTo(var);
  scope->AddDeclaration(decl);
  Statement *stmt =
    factory_.NewExpressionStatement(
      factory_.NewAssignment(Token::INIT_LET, o_proxy, p->obj_, p->position()),
      p->position());
  init->AddStatement(stmt, zone());
  
  VariableProxy *k_proxy;
  k_proxy = scope->NewUnresolved(&factory_,
                                 info_->ast_value_factory()->GetOneByteString("$k"),
                                 Interface::NewValue(),
                                 p->position());
  decl = factory_.NewVariableDeclaration(k_proxy, VariableMode::LET, scope,
                                         p->position());
  var = scope->DeclareLocal(k_proxy->raw_name(), decl->mode(),
                            decl->initialization(),
                            k_proxy->interface());
  var->set_initializer_position(p->position());
  k_proxy->BindTo(var);
  scope->AddDeclaration(decl);
  stmt =
    factory_.NewExpressionStatement(
      factory_.NewAssignment(Token::INIT_LET, k_proxy, p->key_, p->position()),
      p->position());
  init->AddStatement(stmt, zone());

  Expression *key = p->key_->IsLiteral() ? p->key_ : k_proxy;
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(3, zone());
  args->Add(o_proxy, zone());
  args->Add(k_proxy, zone());
  p->obj_ = o_proxy;
  p->key_ = key;
  args->Add(p, zone());
  Call *ret = factory_.NewCall(ER_readProp_proxy_, args, p->position());

  ZoneList<Statement*> *body = new (zone()) ZoneList<Statement*>(3, zone());
  body->Add(init, zone());

  FunctionLiteral* fn = factory_.NewFunctionLiteral(
    info_->ast_value_factory()->empty_string(),
    info_->ast_value_factory(),
    scope,
    body,
    /* materialized_literal_count */ 0,
    /* expected_property_count */ 0,
    /* handler_count */ 0,
    /* num_parameters */ 0,
    FunctionLiteral::kNoDuplicateParameters,
    FunctionLiteral::ANONYMOUS_EXPRESSION,
    FunctionLiteral::kIsFunction,
    FunctionLiteral::kIsParenthesized,
    FunctionLiteral::kNormalFunction,
    p->position());
  rewrite(this, fn);
  body->Add(factory_.NewReturnStatement(ret, p->position()), zone());
  return factory_.NewCall(fn, new(zone()) ZoneList<Expression*>(0, zone()),
                          p->position());
}

Call* EventRacerRewriter::doVisit(Call *c) {
  rewrite(this, c->expression_);
  rewrite(this, c->arguments());
  return c;
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

} } // namespace v8::internal
