#include "src/compiler.h"
#include "src/event-racer-rewriter.h"
#include "src/scopes.h"

namespace v8 {

namespace internal {

EventRacerRewriter::AstRewriterImpl(CompilationInfo *info)
  : info_(info),
    current_context_(NULL),
    factory_(info_->ast_value_factory()),
    arg_names_(NULL) {

  InitializeAstRewriter(info->zone());

  Scope &globals = *info->script_scope();
  AstValueFactory &values = *info->ast_value_factory();

#define FN(fn)                                                          \
  instr_fn_name_[fn] = values.GetOneByteString(#fn);                    \
  instr_fn_[fn] = globals.DeclareDynamicGlobal(instr_fn_name_[fn]);
  INSTRUMENTATION_FUNCTION_LIST(FN)
#undef FN
  o_string_ = values.GetOneByteString("$obj");
  k_string_ = values.GetOneByteString("$key");
  v_string_ = values.GetOneByteString("$value");
  getctx_string_ = values.GetOneByteString("%GetContextN");
}

VariableProxy *EventRacerRewriter::fn_proxy(enum InstrumentationFunction fn) {
  DCHECK(fn < FN_MAX);
  VariableProxy *vp = factory_.NewVariableProxy(instr_fn_[fn]);
  vp->set_do_not_instrument();
  return vp;
}

Expression *EventRacerRewriter::call_runtime(enum InstrumentationFunction fn,
                                             ZoneList<Expression*> *args,
                                             int pos) {
  DCHECK(fn < FN_MAX);
  return factory_.NewCallRuntime(instr_fn_name_[fn], nullptr, args, pos);
}

FunctionLiteral *EventRacerRewriter::make_fn(Scope *scope,
                                             ZoneList<Statement *> *body,
                                             int param_count, int pos) {
  FunctionLiteral *fn = factory_.NewFunctionLiteral(
    info_->ast_value_factory()->empty_string(),
    info_->ast_value_factory(),
    scope,
    body,
    /* materialized_literal_count */ 0,
    /* expected_property_count */ 0,
    /* handler_count */ 0,
    /* num_parameters */ param_count,
    FunctionLiteral::kNoDuplicateParameters,
    FunctionLiteral::ANONYMOUS_EXPRESSION,
    FunctionLiteral::kIsFunction,
    FunctionLiteral::kIsParenthesized,
    FunctionKind::kNormalFunction,
    pos);
  return fn;
}

Expression *EventRacerRewriter::log_vp(VariableProxy *vp, Expression *value,
                                       enum InstrumentationFunction plain_fn,
                                       enum InstrumentationFunction prop_fn) {
  ZoneList<Expression*> *args;
  Variable *var = vp->var();
  if (var == NULL || var->IsLookupSlot() || var->IsUnallocated()) {
    // Read/Write of a property of the global object is rewritten into a
    // call to ER_read/ER_write:
    //
    // |v| => |ER_read("v", v)|
    //  or
    // |v = ex| => |v = ER_write("v", ex)|
    args = new (zone()) ZoneList<Expression*>(2, zone());
    args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()),
              zone());
    args->Add(value, zone());
    if (plain_fn == ER_write && value->IsFunctionLiteral()) {
      // Special case of function literal assignment - call
      // |ER_writeFunc|, passing as a third argument the function
      // literal identifier.
      plain_fn = ER_writeFunc;
      args->Add(
          factory_.NewSmiLiteral(value->AsFunctionLiteral()->function_id(),
                                 RelocInfo::kNoPosition),
          zone());
    }
    return call_runtime(plain_fn, args, vp->position());
  } else {
    // Read/Write of a context allocated variable is rewritten into a call to
    // ER_readProp/ER_writeProp:
    //
    // |v| => ER_readProp(ctx, "v", v)|
    // or
    // |v = ex| => |v = ER_writeProp(ctx, "v", ex)|
    DCHECK(var->IsContextSlot());
    args = new (zone()) ZoneList<Expression*>(1, zone());
    args->Add(
      factory_.NewSmiLiteral(context()->scope->ContextChainLength(var->scope()),
                             RelocInfo::kNoPosition),
      zone());
    CallRuntime *rtcall =
      factory_.NewCallRuntime(getctx_string_,
                              Runtime::FunctionForId(Runtime::kGetContextN),
                              args,
                              vp->position());
    args = new (zone()) ZoneList<Expression*>(3, zone());
    args->Add(rtcall, zone());
    args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()),
              zone());
    args->Add(value, zone());
    return call_runtime(prop_fn, args, vp->position());
  }
}

EventRacerRewriter::ScopeHack *EventRacerRewriter::NewScope(Scope* outer) {
  return new (zone()) ScopeHack(outer ? outer : info_->script_scope(),
                                info_->ast_value_factory(), zone());
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

FunctionDeclaration *EventRacerRewriter::doVisit(FunctionDeclaration *fn) {
  rewrite(this, fn->fun_);
  return fn;
}

Block* EventRacerRewriter::doVisit(Block *blk) {
  ContextScope _(this, blk->scope());
  if (blk->scope())
    rewrite(this, blk->scope()->declarations());
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
  rewrite(this, st->subject_);
  rewrite(this, st->body_);
  return st;
}

ForOfStatement *EventRacerRewriter::doVisit(ForOfStatement *st) {
  rewrite(this, st->subject_);
  rewrite(this, st->body_);
  // TODO(chill) rewrite(this, st->assign_iterator_);
  // TODO(chill) rewrite(this, st->next_result_);
  // TODO(chill) rewrite(this, st->result_done_);
  // TODO(chill) rewrite(this, st->assign_each_);
  return st;
}

ReturnStatement *EventRacerRewriter::doVisit(ReturnStatement *st) {
  rewrite(this, st->expression_);
  ZoneList<Expression *> *args = new(zone()) ZoneList<Expression*>(1, zone());
  args->Add(st->expression_, zone());
  return
      factory_.NewReturnStatement(
          call_runtime(ER_exitFunction, args, st->position()),
          st->position());
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
  // Instrument only access to potentially shared variables, namely
  // those declared by the user (as opposed to being introduced by the
  // compiler), which aren't stack allocated.
  if (!is_potentially_shared(vp))
    return vp;
  return log_vp(vp, vp, ER_read, ER_readProp);
}

Expression* EventRacerRewriter::doVisit(Property *p) {
  rewrite(this, p->obj_);
  rewrite(this, p->key_);

  // Read of a property of an object is rewritten into a call to
  // ER_readProp.

  // If the key is a literal, the Property is rewritten into a call
  // like:

  // |obj.key|
  //  =>
  // |(function($obj) { return ER_readProp($obj, "key", $obj.key); })(obj)|

  // If the key as a general expression, the Property is rewritten into
  // a call to the runtime function ER_readPropIdx:

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
    ScopeHack *scope;
    ZoneList<Statement*> *body;

    DCHECK(obj->position() < p->position());
    scope = NewScope(context()->scope);
    scope->set_start_position(p->position());
    scope->set_end_position(p->position() + 1);

    // Declare the parameter of the new function.
    Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
    o_parm->AllocateTo(Variable::PARAMETER, 0);

    // The |$obj| parameter is referenced in two places, so is the
    // |key| parameter. Create separate AST nodes for each reference.
    Expression *o[2], *k[2];
    o[0] = factory_.NewVariableProxy(o_parm);
    o[1] = factory_.NewVariableProxy(o_parm);
    k[0] = duplicate_key(key->AsLiteral());
    k[1] = duplicate_key(key->AsLiteral());

    // Setup arguments of the call to |ER_readProp|.
    args = new (zone()) ZoneList<Expression*>(3, zone());
    args->Add(o[0], zone());
    args->Add(k[0], zone());
    args->Add(factory_.NewProperty(o[1], k[1], RelocInfo::kNoPosition), zone());

    // Create the return statement.
    body = new (zone()) ZoneList<Statement*>(1, zone());
    body->Add(
        factory_.NewReturnStatement(
            call_runtime(ER_readProp, args, RelocInfo::kNoPosition),
            RelocInfo::kNoPosition),
        zone());

    // Define the new function and build the call.
    FunctionLiteral *fn = make_fn(scope, body, 1 + !is_literal_key(key),
                                  RelocInfo::kNoPosition);
    args = new (zone()) ZoneList<Expression*>(1, zone());
    args->Add(obj, zone());
    return factory_.NewCall(fn, args, p->position() + 1);
  } else {
    // Build a call to |ER_readPropIdx|
    args = new (zone()) ZoneList<Expression*>(2, zone());
    args->Add(obj, zone());
    args->Add(key, zone());
    return call_runtime(ER_readPropIdx, args, obj->position());
  }
}

Call* EventRacerRewriter::doVisit(Call *c) {
  // Non-property calls aren't treated specially.
  if (!c->expression_->IsProperty()) {
    // Do not instrument direct |eval| calls, as the instrumentation
    // changes them into indirect calls and the semantics differ.
    if (c->GetCallType(zone()->isolate()) != Call::POSSIBLY_EVAL_CALL)
      rewrite(this, c->expression_);
    rewrite(this, c->arguments());
    return c;
  }

  // Call of a property is instrumented like:
  //
  // |o.f(e0, e2, ..., en)|
  // =>
  // |(function($obj, $key, $a0, $a1, ..., $an) {|
  // |   ER_readProp($obj, $key, $obj.$key);     |
  // |   return $obj.$key($a0, $a1, ..., $an);   |
  // |})(o, f, e0, e1, ..., en)                  |
  //
  // Property calls are treated by the compiler in a special manner -
  // the callee gets a receiver object - hence we should end up with an
  // instrumented expression, which also contains an analoguos property
  // call.
  Property *p = c->expression()->AsProperty();
  rewrite(this, p->obj_);
  rewrite(this, p->key_);
  rewrite(this, c->arguments());

  ScopeHack *scope;
  ZoneList<Expression*> *args;
  ZoneList<Statement*> *body;
  Expression *obj = p->obj(), *key = p->key();
  const int n = c->arguments()->length();

  DCHECK(obj->position() < p->position());
  DCHECK(p->position() < c->position());
  scope = NewScope(context()->scope);
  scope->set_start_position(p->position());
  scope->set_end_position(p->position() + 1);

  // Declare parameters of the new function.
  Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
  o_parm->AllocateTo(Variable::PARAMETER, 0);

  Variable *k_parm = NULL;
  if (!is_literal_key(key)) {
    k_parm = scope->DeclareParameter(k_string_, VAR);
    k_parm->AllocateTo(Variable::PARAMETER, 1);
  }

  ensure_arg_names(n);
  for (int i = 0; i < n; ++i) {
    Variable *parm = scope->DeclareParameter(arg_names_->at(i), VAR);
    parm->AllocateTo(Variable::PARAMETER, i + 1 + !is_literal_key(key));
  }

  // The |$obj| parameter is referenced in three places, so is the
  // |$key| parameter. Create separate AST nodes for each reference.
  Expression *o[3], *k[3];
  for (int i = 0; i < 3; ++i) {
    o[i] = factory_.NewVariableProxy(o_parm);
    if (is_literal_key(key))
      k[i] = duplicate_key(key->AsLiteral());
    else
      k[i] = factory_.NewVariableProxy(k_parm);
  }

  // Setup arguments of the call to |ER_readProp|.
  args = new (zone()) ZoneList<Expression*>(3, zone());
  args->Add(o[0], zone());
  args->Add(k[0], zone());
  args->Add(factory_.NewProperty(o[1], k[1], RelocInfo::kNoPosition), zone());

  body = new (zone()) ZoneList<Statement*>(1, zone());
  body->Add(
      factory_.NewExpressionStatement(
          call_runtime(ER_readProp, args, RelocInfo::kNoPosition),
          RelocInfo::kNoPosition),
      zone());

  // Build the inner property call.
  args = new (zone()) ZoneList<Expression *>(n, zone());
  for (int i = 0; i < n; ++i) {
    args->Add(
        factory_.NewVariableProxy(
            scope->parameter(i + 1 + !is_literal_key(key))),
        zone());
  }
  body->Add(
      factory_.NewReturnStatement(
          factory_.NewCall(
              factory_.NewProperty(o[2], k[2], RelocInfo::kNoPosition),
              args, RelocInfo::kNoPosition),
          RelocInfo::kNoPosition),
      zone());


  // Define the new function and build the call.
  FunctionLiteral *fn = make_fn(scope, body, n + 1 + !is_literal_key(key),
                                RelocInfo::kNoPosition);
  args = new (zone()) ZoneList<Expression*>(n + 2, zone());
  args->Add(obj, zone());
  if (!is_literal_key(key))
    args->Add(key, zone());
  for (int i = 0; i < n; ++i)
    args->Add(c->arguments()->at(i), zone());

  return factory_.NewCall(fn, args, c->position());
}

CallNew* EventRacerRewriter::doVisit(CallNew *c) {
  rewrite(this, c->expression_);
  rewrite(this, c->arguments());
  return c;
}

CallRuntime* EventRacerRewriter::doVisit(CallRuntime *c) {
  rewrite(this, c->arguments());

  // Global variable initialization is a write, but it's not represented
  // with an assignment in the AST, but with a call to the runtime
  // function |initializeVarGlobal|.
  const Runtime::Function *fn = c->function();
  if (fn && fn->function_id == Runtime::kInitializeVarGlobal) {
    ZoneList<Expression *> *args = new(zone()) ZoneList<Expression*>(2, zone());
    args->Add(duplicate_key(c->arguments()->at(0)->AsLiteral()), zone());
    Expression *value = c->arguments()->at(2);
    args->Add(value, zone());
    enum InstrumentationFunction fn;
    if (value->IsFunctionLiteral()) {
      // Special case of function literal assignment - call
      // |ER_writeFunc|, passing as a third argument the function
      // literal identifier.
      fn = ER_writeFunc;
      args->Add(
          factory_.NewSmiLiteral(value->AsFunctionLiteral()->function_id(),
                                 RelocInfo::kNoPosition),
          zone());
    } else {
      fn = ER_write;
    }
    Expression *call = call_runtime(fn, args, RelocInfo::kNoPosition);
    (*c->arguments())[2] = call;
  }
  return c;
}

Expression* EventRacerRewriter::doVisit(UnaryOperation *op) {
  if (op->op() == Token::DELETE) {
    ScopeHack *scope;
    ZoneList<Expression*> *args;
    ZoneList<Statement*> *body;

    if (op->expression()->IsProperty()) {
      // Rewrite the subtrees.
      Property *p = op->expression_->AsProperty();
      rewrite(this, p->obj_);
      rewrite(this, p->key_);

      // Deletion of a propery is instrumented like:
      //
      // |delete obj.key|
      // =>
      // |(function($obj) {                  |
      // |   ER_deleteProp(obj, "key");      |
      // |   return delete $obj.key;         |
      // |})(obj)                            |
      //
      // or, if the key is a general expression, with a call to the
      // function |ER_deletePropIdx|
      Expression *obj = p->obj_, *key = p->key_;
      if (is_literal_key(key)) {

        DCHECK(op->position() < p->position());
        scope = NewScope(context()->scope);
        scope->set_start_position(op->position());
        scope->set_end_position(op->position() + 1);

        // Declare parameters of the new function.
        Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
        o_parm->AllocateTo(Variable::PARAMETER, 0);

        // The |$obj| parameter is referenced in two places, so is the
        // key. Create separate AST nodes for each reference.
        Expression *o[2], *k[2];
        for (int i = 0; i < 2; ++i) {
          o[i] = factory_.NewVariableProxy(o_parm);
          k[i] = duplicate_key(key->AsLiteral());
        }

        // Setup arguments of the call to |ER_deleteProp|.
        args = new (zone()) ZoneList<Expression*>(2, zone());
        args->Add(o[0], zone());
        args->Add(k[0], zone());

        body = new (zone()) ZoneList<Statement*>(2, zone());
        body->Add(
            factory_.NewExpressionStatement(
                call_runtime(ER_deleteProp, args, RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            zone());

        // Build the inner delete expression.
        body->Add(
            factory_.NewReturnStatement(
                factory_.NewUnaryOperation(
                    Token::DELETE,
                    factory_.NewProperty(o[1], k[1], RelocInfo::kNoPosition),
                    RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            zone());

        // Define the new function and build the call.
        FunctionLiteral *fn = make_fn(scope, body, 1, RelocInfo::kNoPosition);
        args = new (zone()) ZoneList<Expression*>(1, zone());
        args->Add(obj, zone());
        return factory_.NewCall(fn, args, p->position());
      } else {
        args = new (zone()) ZoneList<Expression*>(2, zone());
        args->Add(obj, zone());
        args->Add(key, zone());
        return call_runtime(context()->scope->strict_mode() == SLOPPY
                            ? ER_deletePropIdx : ER_deletePropIdxStrict,
                            args,
                            op->position());
      }
    } else if (op->expression_->IsVariableProxy()) {
      VariableProxy *vp = op->expression_->AsVariableProxy();
      Variable *var = vp->var();
      if (var->IsUnallocated()
          || (!var->IsStackAllocated() && !var->IsContextSlot())) {
        // Instrument delete of a global object property like:
        //
        // |delete v|
        // =>
        // |(function() {      |
        // |   ER_delete("v"); |
        // |   return delete v;|
        // |})();              |
        DCHECK(op->position() < vp->position());
        scope = NewScope(context()->scope);
        scope->set_start_position(op->position());
        scope->set_end_position(op->position() + 1);

        // Call |ER_delete|
        args = new (zone()) ZoneList<Expression*>(1, zone());
        args->Add(factory_.NewStringLiteral(vp->raw_name(), vp->position()),
                  zone());

        body = new (zone()) ZoneList<Statement*>(2, zone());
        body->Add(
            factory_.NewExpressionStatement(
                call_runtime(ER_delete, args, RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            zone());

        // Build the inner delete expression.
        body->Add(
            factory_.NewReturnStatement(
                factory_.NewUnaryOperation(
                    Token::DELETE,
                    factory_.NewVariableProxy(var, vp->position()),
                    RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            zone());

        // Define the new function and build the call.
        FunctionLiteral *fn = make_fn(scope, body, 0, RelocInfo::kNoPosition);
        args = new (zone()) ZoneList<Expression*>(0, zone());
        return factory_.NewCall(fn, args, vp->position());
      } else {
        return op;
      }
    }
  }

  if (op->op() == Token::TYPEOF && op->expression_->IsVariableProxy()) {
    // The |typeof| operator needs special handling since
    // |typeof <some-unknown-identifier>| evaluates to |undefined| instead
    // of throwing an error.
    VariableProxy *vp = op->expression_->AsVariableProxy();
    if (is_potentially_shared(vp))
      return log_vp(vp, op, ER_read, ER_readProp);
  }

  rewrite(this, op->expression_);
  return op;
}

BinaryOperation* EventRacerRewriter::doVisit(BinaryOperation *op) {
  rewrite(this, op->left_);
  rewrite(this, op->right_);
  return op;
}

Expression* EventRacerRewriter::rewriteIncDecVariable(
    Token::Value op, bool is_prefix, VariableProxy *vp, int pos) {
  // Pre-increment/decrement of a variable is instrumented like:
  //
  // |++x|
  // =>
  // |(function() { let $v = ++x; ER_write("x", x); return $v; })()|
  //
  // Post-increment/decrement of a variable is instrumented like:
  //
  // |x++|
  // =>
  // |(function() { let $v = x++; ER_write("x", x); return $v; })()|
  //
  ScopeHack *scope = NewScope(context()->scope);
  scope->set_start_position(vp->position());
  scope->set_end_position(vp->position() + 1);

  ZoneList<Statement*> *body = new (zone()) ZoneList<Statement*>(4, zone());

  // Declare the local variable.
  Variable *value = nullptr;
  value = scope->DeclareLocal(v_string_, LET, kCreatedInitialized,
                              kNotAssigned);
  scope->AllocateStackSlot(value);

  // Initialize the local variable.
  Block *blk = factory_.NewBlock(NULL, 1, true, RelocInfo::kNoPosition);
  blk->AddStatement(
      factory_.NewExpressionStatement(
          factory_.NewAssignment(
              Token::INIT_LET,
              factory_.NewVariableProxy(value),
              factory_.NewCountOperation(
                  op, is_prefix, factory_.NewVariableProxy(vp->var(),
                                                           vp->position()),
                  RelocInfo::kNoPosition),
              RelocInfo::kNoPosition),
          RelocInfo::kNoPosition),
      zone());
  body->Add(blk, zone());

  // Log the write.
  body->Add(
      factory_.NewExpressionStatement(log_vp(vp, vp, ER_write, ER_writeProp),
                                      RelocInfo::kNoPosition),
      zone());

  // Create the return statement.
  body->Add(factory_.NewReturnStatement(factory_.NewVariableProxy(value),
                                        RelocInfo::kNoPosition),
            zone());

  // Define the new function and build the call.
  FunctionLiteral *fn = make_fn(scope, body, 0, RelocInfo::kNoPosition);
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(0, zone());
  return factory_.NewCall(fn, args, pos);
}

Expression *EventRacerRewriter::rewriteIncDecProperty(
    Token::Value op, bool is_prefix, Property *p, int pos) {
  // Pre-increment/decrement of a property is instrumented like:
  //
  // |++obj.key|
  // =>
  // |(function($obj) {                            |
  // |  let $v = ++$obj.key;                       |
  // |  ER_writeProp($obj, "key", $obj.key);       |
  // |  return $v;                                 |
  // |})(obj)                                      |
  //
  // or, if the property name is a general expression, is converted into a
  // call to one of the helper functions |ER_preIncProp| or |ER_preDecProp|
  //
  // |++obj[key]|
  // =>
  // |ER_preIncProp(obj, key)|
  //
  // Post-increment/decrement of a property is instrumented like:
  //
  // |obj.key++|
  // =>
  // |(function($obj) {                            |
  // |  let $v = $obj.key++;                       |
  // |  ER_writeProp($obj, "key", $obj.key);       |
  // |  return $v;                                 |
  // |})(obj)                                      |
  //
  // or, if the property name is a general expression, is converted into a
  // call to one of the helper functions |ER_postIncProp| or |ER_postDecProp|
  //
  // |obj[key]++|
  // =>
  // |ER_postIncProp(obj, key)|
  Expression *obj = p->obj_, *key = p->key_;
  if (is_literal_key(key)) {
    DCHECK(obj->position() < p->position());
    DCHECK(p->position() < pos);

    ScopeHack *scope = NewScope(context()->scope);
    scope->set_start_position(p->position());
    scope->set_end_position(p->position() + 1);

    // Declare the parameter.
    Variable *o_parm = scope->DeclareParameter(o_string_, VAR);
    o_parm->AllocateTo(Variable::PARAMETER, 0);

    // Declare the local variable.
    Variable *value = scope->DeclareLocal(v_string_, LET,
                                          kCreatedInitialized,
                                          kNotAssigned);
    scope->AllocateStackSlot(value);

    // The |$obj| parameter is referenced in three places, so is
    // the property name. The |$v| parameter is referenced
    // once. Create separate AST nodes for each reference.
    Expression *o[3], *k[3];
    for(int i = 0; i < 3; ++i) {
      o[i] = factory_.NewVariableProxy(o_parm);
      k[i] = duplicate_key(key->AsLiteral());
    }

    // Initialize the local variable.
    Block *blk = factory_.NewBlock(NULL, 1, true, RelocInfo::kNoPosition);
    blk->AddStatement(
        factory_.NewExpressionStatement(
            factory_.NewAssignment(
                Token::INIT_LET,
                factory_.NewVariableProxy(value),
                factory_.NewCountOperation(
                    op, is_prefix,
                    factory_.NewProperty(o[0], k[0], RelocInfo::kNoPosition),
                    RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            RelocInfo::kNoPosition),
        zone());

    ZoneList<Statement*> *body = new (zone()) ZoneList<Statement*>(2, zone());
    body->Add(blk, zone());

    // Setup arguments of the call to |ER_writeProp|.
    ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(2, zone());
    args->Add(o[1], zone());
    args->Add(k[1], zone());
    args->Add(factory_.NewProperty(o[2], k[2], RelocInfo::kNoPosition), zone());

    // Create the return statement.
    body->Add(factory_.NewReturnStatement(factory_.NewVariableProxy(value),
                                          RelocInfo::kNoPosition),
              zone());

    // Define the new function and build the call.
    FunctionLiteral *fn = make_fn(scope, body, 1, RelocInfo::kNoPosition);
    args = new (zone()) ZoneList<Expression*>(0, zone());
    args->Add(obj, zone());
    return factory_.NewCall(fn, args, pos);
  } else { // not a literal key
    static InstrumentationFunction fntab[2][2][2] = {
      { // post
        {ER_postIncProp, ER_postIncPropStrict},
        {ER_postDecProp, ER_postDecPropStrict},
      },
      { // pre
        {ER_preIncProp, ER_preIncPropStrict},
        {ER_preDecProp, ER_preDecPropStrict},
      },
    };
    ZoneList<Expression *> *args =
        new (zone()) ZoneList<Expression*>(2, zone());
    args->Add(obj, zone());
    args->Add(key, zone());
    InstrumentationFunction fn =
        fntab[is_prefix][op == Token::DEC][context()->scope->strict_mode() == STRICT];
    return call_runtime(fn, args, pos);
  }
}

Expression* EventRacerRewriter::doVisit(CountOperation *op) {
  DCHECK(op->expression_->IsVariableProxy() || op->expression_->IsProperty());

  // Rewrite subexpressions and check if a we have a potentially shared
  // variable proxy.
  VariableProxy *vp = op->expression_->AsVariableProxy();
  if (vp != NULL) {
    if (!is_potentially_shared(vp))
      return op;
    return rewriteIncDecVariable(op->op(), op->is_prefix(), vp, op->position());
  }

  Property *p = op->expression_->AsProperty();
  rewrite(this, p->obj_);
  rewrite(this, p->key_);
  return rewriteIncDecProperty(op->op(), op->is_prefix(), p, op->position());
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
  DCHECK(op->target_->IsVariableProxy() || op->target_->IsProperty());

  rewrite(this, op->value_);
  if (op->is_compound())
    op->binary_operation_->right_ = op->value_;

  // Do not rewrite the top-level LHS expression.
  if (op->target_->IsProperty()) {
    Property *p = op->target_->AsProperty();
    rewrite(this, p->obj_);
    rewrite(this, p->key_);
  }

  ZoneList<Expression*> *args;
  VariableProxy *vp = op->target_->AsVariableProxy();
  if (vp != NULL) {
    if (!is_potentially_shared(vp))
      return op;

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

    Expression *value;
    if (op->is_compound()) {
      BinaryOperation *binop = op->binary_operation_;
      op->binary_operation_ = NULL;
      op->bit_field_ = Assignment::TokenField::update(op->bit_field_,
                                                      Token::ASSIGN);
      op->target_ = factory_.NewVariableProxy(vp->var(), vp->position());
      value = binop;
    } else {
      value = op->value_;
    }
    op->value_ = log_vp(vp, value, ER_write, ER_writeProp);
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
      ScopeHack *scope;
      ZoneList<Statement*> *body;

      DCHECK(obj->position() < p->position());
      DCHECK(p->position() < op->position());
      scope = NewScope(context()->scope);
      scope->set_start_position(p->position());
      scope->set_end_position(p->position() + 1);

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
      } else {
        o[2] = NULL;
        k[2] = NULL;
      }

      // Setup arguments of the call to |ER_writeProp|.
      enum InstrumentationFunction fn = ER_writeProp;
      args = new (zone()) ZoneList<Expression*>(3, zone());
      args->Add(o[0], zone());
      args->Add(k[0], zone());
      if (op->is_compound()) {
        args->Add(
            factory_.NewBinaryOperation(
                op->binary_op(),
                factory_.NewProperty(o[2], k[2], RelocInfo::kNoPosition),
                v, op->position()),
            zone());
        op->binary_operation_ = NULL;
      } else {
        args->Add(v, zone());
        if (op->value_->IsFunctionLiteral()) {
          // Special case of function literal assignment - call
          // |ER_writePropFunc|, passing as a third argument the
          // function literal identifier.
          fn = ER_writePropFunc;
          args->Add(
              factory_.NewSmiLiteral(
                  op->value_->AsFunctionLiteral()->function_id(),
                  RelocInfo::kNoPosition),
              zone());
        }
      }

      // Create the return statement.
      body = new (zone()) ZoneList<Statement*>(1, zone());
      body->Add(
          factory_.NewReturnStatement(
              factory_.NewAssignment(
                  Token::ASSIGN,
                  factory_.NewProperty(o[1], k[1], RelocInfo::kNoPosition),
                  call_runtime(fn, args, RelocInfo::kNoPosition),
                  RelocInfo::kNoPosition),
              RelocInfo::kNoPosition),
          zone());

      // Define the new function and build the call.
      FunctionLiteral *func = make_fn(scope, body, 2, RelocInfo::kNoPosition);
      args = new (zone()) ZoneList<Expression*>(2, zone());
      args->Add(obj, zone());
      args->Add(op->value_, zone());
      return factory_.NewCall(func, args, op->position());
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
        ScopeHack *scope;
        ZoneList<Statement*> *body;

        DCHECK(obj->position() < p->position());
        DCHECK(p->position() < op->position());
        scope = NewScope(context()->scope);
        scope->set_start_position(p->position());
        scope->set_end_position(p->position() + 1);

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
        args->Add(o[0], zone());
        args->Add(k[0], zone());
        args->Add(
            factory_.NewBinaryOperation(
                op->binary_op(),
                factory_.NewProperty(o[2], k[2], RelocInfo::kNoPosition),
                v, op->position()),
            zone());

        // Create the return statement.
        body = new (zone()) ZoneList<Statement*>(1, zone());
        body->Add(
            factory_.NewReturnStatement(
                factory_.NewAssignment(
                    Token::ASSIGN,
                    factory_.NewProperty(o[1], k[1], RelocInfo::kNoPosition),
                    call_runtime(ER_writeProp, args, RelocInfo::kNoPosition),
                    RelocInfo::kNoPosition),
                RelocInfo::kNoPosition),
            zone());

        // Define the new function and build the call.
        FunctionLiteral *fn = make_fn(scope, body, 3, RelocInfo::kNoPosition);
        args = new (zone()) ZoneList<Expression*>(3, zone());
        args->Add(obj, zone());
        args->Add(key, zone());
        args->Add(op->value_, zone());
        return factory_.NewCall(fn, args, op->position());
      } else {
        enum InstrumentationFunction fn;
        args = new (zone()) ZoneList<Expression*>(3, zone());
        args->Add(obj, zone());
        args->Add(key, zone());
        args->Add(op->value_, zone());
        if (op->value_->IsFunctionLiteral()) {
          args->Add(
              factory_.NewSmiLiteral(
                  op->value_->AsFunctionLiteral()->function_id(),
                  RelocInfo::kNoPosition),
              zone());
          if (context()->scope->strict_mode() == SLOPPY)
            fn = ER_writePropIdxFunc;
          else
            fn = ER_writePropIdxFuncStrict;
        } else {
          if (context()->scope->strict_mode() == SLOPPY)
            fn = ER_writePropIdx;
          else
            fn = ER_writePropIdxStrict;
        }
        return call_runtime(fn, args, op->position());
      }
    }
  }
}

Yield* EventRacerRewriter::doVisit(Yield *op) {
  if (op->yield_kind() == Yield::kSuspend || op->yield_kind() == Yield::kFinal)
    rewrite(this, op->expression_);
  return op;
}

Throw* EventRacerRewriter::doVisit(Throw *op) {
  rewrite(this, op->exception_);
  return op;
}

FunctionLiteral* EventRacerRewriter::doVisit(FunctionLiteral *lit) {
  if (!info_->shared_info().is_null())
    lit->set_function_id(info_->shared_info()->function_id());
  else
    lit->initialize_function_id(zone());

  ContextScope _(this, lit->scope());
  rewrite(this, lit->scope()->declarations());
  if (!lit->body() || lit->body()->length() == 0)
    return lit;
  rewrite(this, lit->body());

  ZoneList<Declaration*> &dcls = *lit->scope()->declarations();
  for (int i = 0; i < dcls.length(); ++i) {
    if (dcls[i]->IsFunctionDeclaration()) {
      FunctionDeclaration *fndcl = dcls[i]->AsFunctionDeclaration();
      ZoneList<Expression*> *args =
        new (zone()) ZoneList<Expression*>(3, zone());
      args->Add(factory_.NewStringLiteral(fndcl->proxy()->raw_name(),
                                          fndcl->proxy()->position()),
                zone());
      args->Add(factory_.NewNullLiteral(RelocInfo::kNoPosition), zone());
      args->Add(factory_.NewSmiLiteral(fndcl->fun()->function_id(),
                                       RelocInfo::kNoPosition),
                zone());
      lit->body()->InsertAt(
          0,
          factory_.NewExpressionStatement(
              call_runtime(ER_writeFunc, args, lit->position()),
              lit->position()),
          zone());
    }
  }

  // Emit a statement to log a function entry.
  int scriptId = -1, startLine = -1, endLine = -1;
  if (!info_->script().is_null()) {
    scriptId = info_->script()->id()->value();
    startLine = Script::GetLineNumber(info_->script(),
                                      lit->scope()->start_position())
                - info_->script()->line_offset()->value();
    endLine = Script::GetLineNumber(info_->script(),
                                    lit->scope()->end_position())
              - info_->script()->line_offset()->value();
  }
  ZoneList<Expression*> *args = new (zone()) ZoneList<Expression*>(3, zone());
  if (lit->raw_name()->length()) {
    args->Add(factory_.NewStringLiteral(lit->raw_name(), lit->position()),
              zone());
  } else {
    args->Add(factory_.NewNullLiteral(RelocInfo::kNoPosition), zone());
  }
  args->Add(factory_.NewSmiLiteral(scriptId, RelocInfo::kNoPosition), zone());
  args->Add(factory_.NewSmiLiteral(lit->function_id(), RelocInfo::kNoPosition),
            zone());
  args->Add(factory_.NewSmiLiteral(startLine, RelocInfo::kNoPosition), zone());
  args->Add(factory_.NewSmiLiteral(endLine, RelocInfo::kNoPosition), zone());
  Statement *st =
      factory_.NewExpressionStatement(
          call_runtime(ER_enterFunction, args, lit->position()),
          lit->position());
  lit->body()->InsertAt(0, st, zone());

  // Emit a statement to log the function exit, if the last statement is
  // not a return. It it not a problem if this statement turns out to
  // be dead code.
  if (!lit->body()->last()->IsReturnStatement()) {
    ZoneList<Expression *> *args = new(zone()) ZoneList<Expression*>(1, zone());
    args->Add(factory_.NewUndefinedLiteral(RelocInfo::kNoPosition), zone());
    lit->body()->Add(
        factory_.NewReturnStatement(
            call_runtime(ER_exitFunction, args, RelocInfo::kNoPosition),
            RelocInfo::kNoPosition),
        zone());
  }
  return lit;
}


// -----------------------------------------------------------------------------
void AstSlotCounter::add_materialized_literal(MaterializedLiteral *lit) {
  lit->set_literal_index(state_->materialized_literal_count++);
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
  V(ModulePath)                                 \
  V(DebuggerStatement)                          \
  V(ContinueStatement)                          \
  V(BreakStatement)                             \
  V(EmptyStatement)                             \
  V(Literal)                                    \
  V(NativeFunctionLiteral)                      \
  V(ThisFunction)


#define LEAF_VISIT(type)                        \
  void AstSlotCounter::Visit##type(type *) {}
  AST_LEAF_NODE_LIST(LEAF_VISIT)
#undef LEAF_VISIT

void AstSlotCounter::VisitVariableDeclaration(VariableDeclaration *dcl) {
  traverse(this, dcl->proxy());
}

void AstSlotCounter::VisitFunctionDeclaration(FunctionDeclaration *dcl) {
  traverse(this, dcl->proxy());
  traverse(this, dcl->fun());
}

void AstSlotCounter::VisitModuleDeclaration(ModuleDeclaration *dcl) {
  traverse(this, dcl->proxy());
  traverse(this, dcl->module());
}

void AstSlotCounter::VisitImportDeclaration(ImportDeclaration *dcl) {
  traverse(this, dcl->proxy());
  // FIXME: traverse(this, dcl->module());
}

void AstSlotCounter::VisitExportDeclaration(ExportDeclaration *dcl) {
  traverse(this, dcl->proxy());
}

void AstSlotCounter::VisitModuleLiteral(ModuleLiteral *mod) {
  traverse(this, mod->body());
}

void AstSlotCounter::VisitModuleVariable(ModuleVariable *mod) {
  traverse(this, mod->proxy());
}

void AstSlotCounter::VisitModuleStatement(ModuleStatement *st) {
  traverse(this, st->proxy());
  traverse(this, st->body());
}

void AstSlotCounter::VisitBlock(Block *blk) {
  if (blk->scope())
    traverse(this, blk->scope()->declarations());
  traverse(this, blk->statements());
}

void AstSlotCounter::VisitExpressionStatement(ExpressionStatement *st) {
  traverse(this, st->expression());
}

void AstSlotCounter::VisitDoWhileStatement(DoWhileStatement *st) {
  traverse(this, st->cond());
  traverse(this, st->body());
}

void AstSlotCounter::VisitWhileStatement(WhileStatement *st) {
  traverse(this, st->cond());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForStatement(ForStatement *st) {
  traverse(this, st->init());
  traverse(this, st->cond());
  traverse(this, st->next());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForInStatement(ForInStatement *st) {
  traverse(this, st->each());
  traverse(this, st->subject());
  traverse(this, st->body());
}

void AstSlotCounter::VisitForOfStatement(ForOfStatement *st) {
  traverse(this, st->each());
  traverse(this, st->subject());
  traverse(this, st->body());
  traverse(this, st->assign_iterator());
  traverse(this, st->next_result());
  traverse(this, st->result_done());
  traverse(this, st->assign_each());
}

void AstSlotCounter::VisitReturnStatement(ReturnStatement *st) {
  traverse(this, st->expression());
}

void AstSlotCounter::VisitWithStatement(WithStatement *st) {
  traverse(this, st->expression());
  traverse(this, st->statement());
}

void AstSlotCounter::VisitCaseClause(CaseClause *ex) {
  if (!ex->is_default())
    traverse(this, ex->label());
  traverse(this, ex->statements());
}

void AstSlotCounter::VisitSwitchStatement(SwitchStatement *st) {
  traverse(this, st->tag());
  traverse(this, st->cases());
}

void AstSlotCounter::VisitIfStatement(IfStatement *st) {
  traverse(this, st->condition());
  traverse(this, st->then_statement());
  traverse(this, st->else_statement());
}

void AstSlotCounter::VisitTryCatchStatement(TryCatchStatement *st) {
  traverse(this, st->try_block());
  traverse(this, st->catch_block());
}

void AstSlotCounter::VisitTryFinallyStatement(TryFinallyStatement *st) {
  traverse(this, st->try_block());
  traverse(this, st->finally_block());
}

void AstSlotCounter::VisitObjectLiteral(ObjectLiteral *lit) {
  add_materialized_literal(lit);
  if (lit->properties()) {
    ZoneList<ObjectLiteral::Property *> &ps = *lit->properties();
    for (int i = 0; i < ps.length(); ++i)
      traverse(this, ps[i]->value());
  }
}

void AstSlotCounter::VisitArrayLiteral(ArrayLiteral *lit) {
  add_materialized_literal(lit);
  traverse(this, lit->values());
}

void AstSlotCounter::VisitVariableProxy(VariableProxy *vp) {
}

void AstSlotCounter::VisitProperty(Property *p) {
  traverse(this, p->obj());
  traverse(this, p->key());
}

void AstSlotCounter::VisitCall(Call *c) {
  traverse(this, c->expression());
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitCallNew(CallNew *c) {
  traverse(this, c->expression());
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitCallRuntime(CallRuntime *c) {
  traverse(this, c->arguments());
}

void AstSlotCounter::VisitUnaryOperation(UnaryOperation *op) {
  traverse(this, op->expression());
}

void AstSlotCounter::VisitBinaryOperation(BinaryOperation *op) {
  traverse(this, op->left());
  traverse(this, op->right());
}

void AstSlotCounter::VisitCountOperation(CountOperation *op) {
  traverse(this, op->expression());
}

void AstSlotCounter::VisitCompareOperation(CompareOperation *op) {
  traverse(this, op->left());
  traverse(this, op->right());
}

void AstSlotCounter::VisitSuperReference(SuperReference *sup) {
  traverse(this, sup->this_var());
}

void AstSlotCounter::VisitConditional(Conditional *op) {
  traverse(this, op->condition());
  traverse(this, op->then_expression());
  traverse(this, op->else_expression());
}

void AstSlotCounter::VisitAssignment(Assignment *op) {
  if (op->is_compound())
    traverse(this, op->binary_operation());
  else {
    traverse(this, op->target());
    traverse(this, op->value());
  }
}

void AstSlotCounter::VisitYield(Yield *op) {
  // FIXME:  if (op->yield_kind() == Yield::SUSPEND || op->yield_kind() == Yield::FINAL)
  traverse(this, op->expression());
}

void AstSlotCounter::VisitThrow(Throw *op) {
  traverse(this, op->exception());
}

void AstSlotCounter::VisitRegExpLiteral(RegExpLiteral *lit) {
  add_materialized_literal(lit);
}

void AstSlotCounter::VisitFunctionLiteral(FunctionLiteral *fn) {
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

  end_function();
}

void AstSlotCounter::VisitClassLiteral(ClassLiteral *cls) {
  traverse(this, cls->constructor());
  if (cls->properties()) {
    ZoneList<ClassLiteral::Property *> &ps = *cls->properties();
    for (int i = 0; i < ps.length(); ++i)
      traverse(this, ps[i]->value());
  }
}

AstSlotCounter::FunctionState AstSlotCounter::FunctionState::guard;

} } // namespace v8::internal
