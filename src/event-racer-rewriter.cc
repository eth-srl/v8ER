#include "src/compiler.h"
#include "src/event-racer-rewriter.h"
#include "src/scopes.h"

namespace v8 {

namespace internal {

EventRacerRewriter::AstRewriterImpl(CompilationInfo *info)
  : info_(info), post_scope_analysis_(false),
    current_context_(NULL),
    id_alloc_scope_(NULL),
    factory_(info_->zone(), info_->ast_value_factory()),
    arg_names_(NULL) {

  InitializeAstRewriter(info->zone());

  Scope &globals = *info->global_scope();
  AstValueFactory &values = *info->ast_value_factory();

  ER_read_ = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_read"));
  ER_readProp_ =
    globals.DeclareDynamicGlobal(values.GetOneByteString("ER_readProp"));
  ER_write_ = globals.DeclareDynamicGlobal(values.GetOneByteString("ER_write"));
  ER_writeProp_ =
    globals.DeclareDynamicGlobal(values.GetOneByteString("ER_writeProp"));
  ER_readPropIdx_ =
    globals.DeclareDynamicGlobal(values.GetOneByteString("ER_readPropIdx"));
  ER_writePropIdx_ =
    globals.DeclareDynamicGlobal(values.GetOneByteString("ER_writePropIdx"));
  o_string_ = values.GetOneByteString("$obj");
  k_string_ = values.GetOneByteString("$key");
  v_string_ = values.GetOneByteString("$value");
}

VariableProxy *EventRacerRewriter::ER_read_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_read_);
  vp->set_do_not_instrument();
  return vp;
}

VariableProxy *EventRacerRewriter::ER_readProp_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_readProp_);
  vp->set_do_not_instrument();
  return vp;
}

VariableProxy *EventRacerRewriter::ER_write_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_write_);
  vp->set_do_not_instrument();
  return vp;
}

VariableProxy *EventRacerRewriter::ER_writeProp_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_writeProp_);
  vp->set_do_not_instrument();
  return vp;
}

VariableProxy *EventRacerRewriter::ER_readPropIdx_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_readPropIdx_);
  vp->set_do_not_instrument();
  return vp;
}

VariableProxy *EventRacerRewriter::ER_writePropIdx_proxy(Scope *scope) {
  VariableProxy *vp = factory_.NewVariableProxy(ER_writePropIdx_);
  vp->set_do_not_instrument();
  return vp;
}

Scope *EventRacerRewriter::NewScope(Scope* outer, ScopeType type) {
  Scope* s = new (zone()) Scope(outer ? outer : info_->global_scope(),
                                type, info_->ast_value_factory(),
                                zone());
  s->Initialize();
  s->ForceEagerCompilation();
  return s;
}

VariableProxy *EventRacerRewriter::NewProxy(Scope *scope,
                                            const AstRawString *name,
                                            int pos) {
  return scope->NewUnresolved(&factory_, name, Interface::NewValue(), pos);
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

int EventRacerRewriter::ast_node_id() const {
  return isolate()->ast_node_id();
}

void EventRacerRewriter::set_ast_node_id(int id) {
  isolate()->set_ast_node_id(id);
}

bool EventRacerRewriter::is_literal_key(const Expression *ex) const {
  if (const Literal *lit = ex->AsLiteral()) {
    const AstValue *v = lit->raw_value();
    return v->IsString() || v->IsNumber();
  }
  return false;
}

Literal *EventRacerRewriter::duplicate_key(const Literal *lit) {
  const AstValue *value = lit->raw_value();
  if (value->IsString())
    return factory_.NewStringLiteral(value->AsString(), lit->position());
  else if (value->IsSmi())
    return factory_.NewSmiLiteral(value->AsSmi(), lit->position());
  else if (value->IsNumber())
    return factory_.NewNumberLiteral(value->AsNumber(), lit->position());
  UNREACHABLE();
  return NULL;
}

template<typename T> void rewrite(AstRewriter *w, T *&node) {
  if (node)
    node = node->Accept(w);
}

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
  if (!post_scope_analysis_)
    return vp;

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
  return factory_.NewCall(ER_read_proxy(context()->scope), args, vp->position());
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
  //  =>
  // |(function($obj) { return ER_readProp($obj, "key", $obj.key); })(obj)|

  // If the key as a general expression, the Property is rewritten into
  // a call to the runtime fucntion ER_readPropIdx:

  // |arr[idx]|
  // =>
  // ER_readPropIdx(arr, idx)

  // The function literal is needed in order to ensure |obj| and |key|
  // are evaluated exactly once. Passing the expressions as parameters
  // avoids capturing variabes in the inner closure, which may cause
  // them to be context allocated.

  ZoneList<Expression*> *args;
  Expression *obj = p->obj(), *key = p->key();

  if (is_literal_key(key)) {
      Scope *scope;
      ZoneList<Statement*> *body;
      int fn_ast_node_id;
      {
        AstNodeIdAllocationScope _(this);

        scope = NewScope(context()->scope, FUNCTION_SCOPE);
        scope->set_start_position(p->position());
        scope->set_end_position(p->position() + 1);

        // Declare the parameter of the new function.
        scope->DeclareParameter(o_string_, VAR);

        // The |$obj| parameter is referenced in two places, so is the
        // |key| parameter. Create separate AST nodes for each reference.
        Expression *o[2], *k[2];
        o[0] = NewProxy(scope, o_string_, p->position());
        o[1] = NewProxy(scope, o_string_, p->position());
        k[0] = duplicate_key(key->AsLiteral());
        k[1] = duplicate_key(key->AsLiteral());

        // Setup arguments of the call to |ER_readProp|.
        args = new (zone()) ZoneList<Expression*>(3, zone());

        // First argument is the |$obj| parameter.
        args->Add(o[0], zone());

        // Second argument is the literal/property-name.
        args->Add(k[0], zone());

        // Third argument is the property expression.
        args->Add(factory_.NewProperty(o[1], k[1], p->position()), zone());

        // Create the return statement.
        body = new (zone()) ZoneList<Statement*>(1, zone());
        body->Add(factory_.NewReturnStatement(
                    factory_.NewCall(ER_readProp_proxy(scope), args,
                                     p->position()),
                    p->position()),
                  zone());
        fn_ast_node_id = ast_node_id();
      } // end allocation scope

      // Define the new function.
      FunctionLiteral *fn = factory_.NewFunctionLiteral(
        info_->ast_value_factory()->empty_string(),
        info_->ast_value_factory(),
        scope,
        body,
        /* materialized_literal_count */ 0,
        /* expected_property_count */ 0,
        /* handler_count */ 0,
        /* num_parameters */ 1 + !is_literal_key(key),
        FunctionLiteral::kNoDuplicateParameters,
        FunctionLiteral::ANONYMOUS_EXPRESSION,
        FunctionLiteral::kIsFunction,
        FunctionLiteral::kIsParenthesized,
        FunctionLiteral::kNormalFunction,
        p->position());
      fn->set_next_ast_node_id(fn_ast_node_id);

      // Build the call to the new function.
      args = new (zone()) ZoneList<Expression*>(2, zone());
      args->Add(obj, zone());
      return factory_.NewCall(fn, args, p->position());
    } else {
      // Build a call to |ER_readPropIdx|
      args = new (zone()) ZoneList<Expression*>(2, zone());
      args->Add(obj, zone());
      args->Add(key, zone());
      return factory_.NewCall(ER_readPropIdx_proxy(context()->scope), args,
                              p->position());
    }
}

Call* EventRacerRewriter::doVisit(Call *c) {
  // Calls are rewritten pre scope analysis and non-property-calls
  // aren't treated specially.
  if (post_scope_analysis_ || !c->expression_->IsProperty()) {
    // Do not instrument direct |eval| calls, as the instrumentation
    // changes them into indirect calls and the semantics differ.
    if (c->GetCallType(zone()->isolate()) != Call::POSSIBLY_EVAL_CALL)
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

  Scope *scope;
  ZoneList<Expression*> *args;
  ZoneList<Statement*> *body;
  int fn_ast_node_id;
  Property *p = c->expression()->AsProperty();
  Expression *obj = p->obj(), *key = p->key();
  const int n = c->arguments()->length();
  {
    AstNodeIdAllocationScope _(this);

    scope = NewScope(context()->scope, FUNCTION_SCOPE);
    scope->set_start_position(c->position());
    scope->set_end_position(c->position() + 1);

    // Declare parameters of the new function.
    ensure_arg_names(n);
    for (int i = 0; i < n; ++i)
      scope->DeclareParameter(arg_names_->at(i), VAR);
    scope->DeclareParameter(o_string_, VAR);
    if (!is_literal_key(key))
      scope->DeclareParameter(k_string_, VAR);

    // The |$obj| parameter is referenced in three places, so is the
    // |$key| parameter. Create separate AST nodes for each reference.
    Expression *o[3], *k[3];
    o[0] = NewProxy(scope, o_string_, p->position());
    o[1] = NewProxy(scope, o_string_, p->position());
    o[2] = NewProxy(scope, o_string_, p->position());

    if (is_literal_key(key)) {
      k[0] = duplicate_key(key->AsLiteral());
      k[1] = duplicate_key(key->AsLiteral());
      k[2] = duplicate_key(key->AsLiteral());
    } else {
      k[0] = NewProxy(scope, k_string_, c->position());
      k[1] = NewProxy(scope, k_string_, c->position());
      k[2] = NewProxy(scope, k_string_, c->position());
    }

    // Setup arguments of the call to |ER_readProp|.
    args = new (zone()) ZoneList<Expression*>(3, zone());

    // First argument is the |$obj| parameter.
    args->Add(o[0], zone());

    // Second argument is either the |$key| parameter or, if the
    // property name is a literal, the literal itself.
    args->Add(k[0], zone());

    // Third argument is the property expression.
    args->Add(factory_.NewProperty(o[1], k[1], p->position()), zone());

    body = new (zone()) ZoneList<Statement*>(1, zone());
    body->Add(factory_.NewExpressionStatement(
                factory_.NewCall(ER_readProp_proxy(scope), args, c->position()),
                c->position()),
              zone());

    // Build the inner property call.
    args = new (zone()) ZoneList<Expression *>(n, zone());
    for (int i = 0; i < n; ++i)
      args->Add(NewProxy(scope, arg_names_->at(i), c->position()), zone());
    body->Add(factory_.NewReturnStatement(
                factory_.NewCall(factory_.NewProperty(o[2], k[2], p->position()),
                                 args, c->position()),
                c->position()),
              zone());

    fn_ast_node_id = ast_node_id();
  }

  // Define the new function.
  FunctionLiteral *fn = factory_.NewFunctionLiteral(
    info_->ast_value_factory()->empty_string(),
    info_->ast_value_factory(),
    scope,
    body,
    /* materialized_literal_count */ 0,
    /* expected_property_count */ 0,
    /* handler_count */ 0,
    /* num_parameters */ n + 1 + !is_literal_key(key),
    FunctionLiteral::kNoDuplicateParameters,
    FunctionLiteral::ANONYMOUS_EXPRESSION,
    FunctionLiteral::kIsFunction,
    FunctionLiteral::kIsParenthesized,
    FunctionLiteral::kNormalFunction,
    c->position());
  fn->set_next_ast_node_id(fn_ast_node_id);

  // Build the call to the new function.
  args = new (zone()) ZoneList<Expression*>(n + 2, zone());
  for (int i = 0; i < n; ++i)
    args->Add(c->arguments()->at(i), zone());
  args->Add(obj, zone());
  if (!is_literal_key(key))
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
  if (op->op() == Token::DELETE && op->expression()->IsProperty())
    return op;

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

Expression* EventRacerRewriter::doVisit(Assignment *op) {
  rewrite(this, op->value_);
  if (op->is_compound())
    op->binary_operation_->right_ = op->value_;

  // Do not rewrite the top-level LHS expression.
  if (op->target_->IsProperty()) {
    Property *p = op->target_->AsProperty();
    rewrite(this, p->obj_);
    rewrite(this, p->key_);
  }

  DCHECK(op->target_->IsVariableProxy() || op->target_->IsProperty());
  VariableProxy *vp = op->target_->AsVariableProxy();
  if (!post_scope_analysis_ || (vp != NULL && !is_potentially_shared(vp))) {
    // Assignment expressions are rewritten post scope analysis. Pre
    // scope analysis just descend into the AST.
    return op;
  }

  ZoneList<Expression*> *args;

  if (vp != NULL) {
    // If the LHS is a simple variable, the assignment is rewritten
    // like:
    //
    // |v = e|
    // =>
    // |v = ER_write("v", e);|
    //
    // or, if, it's a compound assignment, like:
    //
    // |v += e|
    // =>
    // |v = ER_write("v", v + e);|

    args = new (zone()) ZoneList<Expression*>(2, zone());
    args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()),
              zone());
    if (op->is_compound()) {
      BinaryOperation *binop = op->binary_operation_;
      op->binary_operation_ = NULL;
      op->op_ = Token::ASSIGN;
      op->target_ = factory_.NewVariableProxy(vp->var());
      args->Add(binop, zone());
    } else {
      args->Add(op->value_, zone());
    }
    op->value_ = factory_.NewCall(ER_write_proxy(context()->scope), args,
                                  op->position());
    return op;
  } else {
    DCHECK(op->target_->IsProperty());
    Property *p = op->target_->AsProperty();
    Expression *obj = p->obj_, *key = p->key_;
    if (is_literal_key(key)) {
      // If the LHS is a property expression with a literal key, the
      // assignment is rewritten like:
      //
      // |obj.key = e|
      // =>
      // |function($obj, $v) {                              |
      // |  return $obj.key = ER_writeProp($obj, "key", $v);|
      // |}                                                 |
      //
      // or, if it's a compound assignment, like:
      //
      // |obj.key += e|
      // =>
      // |function($obj, $v) {                                         |
      // |  return $obj.key = ER_writeProp($obj, "key", $obj.key + $v);|
      // |}                                                            |

      Scope *scope;
      ZoneList<Statement*> *body;
      int fn_ast_node_id;
      {
        AstNodeIdAllocationScope _(this);

        scope = NewScope(context()->scope, FUNCTION_SCOPE);
        scope->set_start_position(op->position());
        scope->set_end_position(op->position() + 1);

        // Declare the parameters of the new function.
        Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
        Variable *v_parm = scope->DeclareParameter(v_string_, VAR);
        o_parm->AllocateTo(Variable::PARAMETER, 0);
        v_parm->AllocateTo(Variable::PARAMETER, 1);

        // The |$obj| parameter is referenced in two (or three) places,
        // so is the property name. The |$v| parameter is referenced
        // once. Create separate AST nodes for each reference.
        Expression *o[3], *k[3], *v;
        o[0] = factory_.NewVariableProxy(o_parm);
        o[1] = factory_.NewVariableProxy(o_parm);
        k[0] = duplicate_key(key->AsLiteral());
        k[1] = duplicate_key(key->AsLiteral());
        v = factory_.NewVariableProxy(v_parm);
        if (op->is_compound()) {
          o[2] = factory_.NewVariableProxy(o_parm);
          k[2] = duplicate_key(key->AsLiteral());
        }

        // Setup arguments of the call to |ER_writeProp|.
        args = new (zone()) ZoneList<Expression*>(3, zone());

        // First argument is the |$obj| parameter.
        args->Add(o[0], zone());

        // Second argument is the literal/property-name.
        args->Add(k[0], zone());

        // Third argument is RHS value.
        if (op->is_compound()) {
          args->Add(factory_.NewBinaryOperation(
                      op->binary_op(),
                      factory_.NewProperty(o[2], k[2], p->position()),
                      v, op->binary_operation_->position()),
                    zone());
          op->binary_operation_ = NULL;
        } else {
          args->Add(v, zone());
        }

        // Create the return statement.
        body = new (zone()) ZoneList<Statement*>(1, zone());
        body->Add(factory_.NewReturnStatement(
                    factory_.NewAssignment(
                      Token::ASSIGN,
                      factory_.NewProperty(o[1], k[1], p->position()),
                      factory_.NewCall(ER_writeProp_proxy(scope), args,
                                       op->position()),
                      op->position()),
                    op->position()),
                  zone());
        fn_ast_node_id = ast_node_id();
      } // end allocation scope

      // Define the new function.
      FunctionLiteral *fn = factory_.NewFunctionLiteral(
        info_->ast_value_factory()->empty_string(),
        info_->ast_value_factory(),
        scope,
        body,
        /* materialized_literal_count */ 0,
        /* expected_property_count */ 0,
        /* handler_count */ 0,
        /* num_parameters */ 2,
        FunctionLiteral::kNoDuplicateParameters,
        FunctionLiteral::ANONYMOUS_EXPRESSION,
        FunctionLiteral::kIsFunction,
        FunctionLiteral::kIsParenthesized,
        FunctionLiteral::kNormalFunction,
        op->position());
      fn->set_next_ast_node_id(fn_ast_node_id);

      // Build the call to the new function.
      args = new (zone()) ZoneList<Expression*>(2, zone());
      args->Add(obj, zone());
      args->Add(op->value_, zone());
      return factory_.NewCall(fn, args, p->position());
    } else {
      // If the LHS is a property expression and the key is a general
      // expression, the assignment is rewritten like:
      //
      // |arr[idx] = e|
      // =>
      // |ER_writePropIdx(arr, idx, e)|
      //
      // or, if it's a compound assignment, like:
      //
      // |arr[idx] += e|
      // =>
      // |(function($obj, $key, $v) {                                      |
      //     return $obj[$key] = ER_writeProp($obj, $key, $obj[$key] + $v);|
      // })(arr, idx, e);                                                  |
      if (op->is_compound()) {
        Scope *scope;
        ZoneList<Statement*> *body;
        int fn_ast_node_id;
        {
          AstNodeIdAllocationScope _(this);

          scope = NewScope(context()->scope, FUNCTION_SCOPE);
          scope->set_start_position(op->position());
          scope->set_end_position(op->position() + 1);

          // Declare the parameters of the new function.
          Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
          Variable *k_parm = scope->DeclareParameter(k_string_, VAR);
          Variable *v_parm = scope->DeclareParameter(v_string_, VAR);
          o_parm->AllocateTo(Variable::PARAMETER, 0);
          k_parm->AllocateTo(Variable::PARAMETER, 1);
          v_parm->AllocateTo(Variable::PARAMETER, 2);

          // The |$obj| parameter is referenced in three places, so is
          // the |$key| parameter. The |$v| parameter is referenced
          // once. Create separate AST nodes for each reference.
          Expression *o[3], *k[3], *v;
          for(int i = 0; i < 3; ++i) {
            o[i] = factory_.NewVariableProxy(o_parm);
            k[i] = factory_.NewVariableProxy(k_parm);
          }
          v = factory_.NewVariableProxy(v_parm);

          // Setup arguments of the call to |ER_writeProp|.
          args = new (zone()) ZoneList<Expression*>(3, zone());

          // First argument is the |$obj| parameter.
          args->Add(o[0], zone());

          // Second argument is the |$key| parameter.
          args->Add(k[0], zone());

          // Third argument is RHS value.
          args->Add(factory_.NewBinaryOperation(
                      op->binary_op(),
                      factory_.NewProperty(o[2], k[2], p->position()),
                      v, op->binary_operation_->position()),
                    zone());

          // Create the return statement.
          body = new (zone()) ZoneList<Statement*>(1, zone());
          body->Add(factory_.NewReturnStatement(
                      factory_.NewAssignment(
                        Token::ASSIGN,
                        factory_.NewProperty(o[1], k[1], p->position()),
                        factory_.NewCall(ER_writeProp_proxy(scope), args,
                                         op->position()),
                        op->position()),
                      op->position()),
                    zone());
          fn_ast_node_id = ast_node_id();
        } // end allocation scope

        // Define the new function.
        FunctionLiteral *fn = factory_.NewFunctionLiteral(
          info_->ast_value_factory()->empty_string(),
          info_->ast_value_factory(),
          scope,
          body,
          /* materialized_literal_count */ 0,
          /* expected_property_count */ 0,
          /* handler_count */ 0,
          /* num_parameters */ 3,
          FunctionLiteral::kNoDuplicateParameters,
          FunctionLiteral::ANONYMOUS_EXPRESSION,
          FunctionLiteral::kIsFunction,
          FunctionLiteral::kIsParenthesized,
          FunctionLiteral::kNormalFunction,
          op->position());
        fn->set_next_ast_node_id(fn_ast_node_id);

        // Build the call to the new function.
        args = new (zone()) ZoneList<Expression*>(3, zone());
        args->Add(obj, zone());
        args->Add(key, zone());
        args->Add(op->value_, zone());
        return factory_.NewCall(fn, args, p->position());
      } else {
        args = new (zone()) ZoneList<Expression*>(3, zone());
        args->Add(obj, zone());
        args->Add(key, zone());
        args->Add(op->value_, zone());
        return factory_.NewCall(ER_writePropIdx_proxy(context()->scope), args,
                                op->position());
      }
    }
  }
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
  AstNodeIdAllocationScope __(this, lit);
  rewrite(this, lit->body());
  lit->set_next_ast_node_id(ast_node_id());
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
  if (op->is_compound())
    traverse(this, op->binary_operation());
  else {
    traverse(this, op->target());
    traverse(this, op->value());
  }
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
  add_node();

  if (!fn->body())
    return;

  FunctionState state;
  begin_function(&state);
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

AstSlotCounter::FunctionState AstSlotCounter::FunctionState::guard;

} } // namespace v8::internal
