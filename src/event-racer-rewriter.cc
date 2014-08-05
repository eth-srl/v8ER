#include "src/event-racer-rewriter.h"

namespace v8 {

namespace internal {

Block* EventRacerRewriter::doVisit(Block *blk) {
  ZoneList<Statement *> &stmts = *blk->statements();
  for(int i = 0; i < stmts.length(); ++i)
    stmts[i] = stmts[i]->Accept(this);
  return blk;
}

ExpressionStatement *EventRacerRewriter::doVisit(ExpressionStatement *st) {
  st->expression_ = st->expression_->Accept(this);
  return st;
}

DoWhileStatement *EventRacerRewriter::doVisit(DoWhileStatement *st) {
  st->cond_ = st->cond_->Accept(this);
  st->body_ = st->body_->Accept(this);
  return st;
}

WhileStatement *EventRacerRewriter::doVisit(WhileStatement *st) {
  st->cond_ = st->cond_->Accept(this);
  st->body_ = st->body_->Accept(this);
  return st;
}

ForStatement *EventRacerRewriter::doVisit(ForStatement *st) {
  st->init_ = st->init_->Accept(this);
  st->cond_ = st->cond_->Accept(this);
  st->next_ = st->next_->Accept(this);
  st->body_ = st->body_->Accept(this);
  return st;
}

ForInStatement *EventRacerRewriter::doVisit(ForInStatement *st) {
  st->each_ = st->each_->Accept(this);
  st->subject_ = st->subject_->Accept(this);
  st->body_ = st->body_->Accept(this);
  return st;
}

ForOfStatement *EventRacerRewriter::doVisit(ForOfStatement *st) {
  st->each_ = st->each_->Accept(this);
  st->subject_ = st->subject_->Accept(this);
  st->body_ = st->body_->Accept(this);
  st->assign_iterable_ = st->assign_iterable_->Accept(this);
  st->assign_iterator_ = st->assign_iterator_->Accept(this);
  st->next_result_ = st->next_result_->Accept(this);
  st->result_done_ = st->result_done_->Accept(this);
  st->assign_each_ = st->assign_each_->Accept(this);
  return st;
}

ContinueStatement *EventRacerRewriter::doVisit(ContinueStatement *st) {
  st->target_ = st->target_->Accept(this);
  return st;
}

BreakStatement *EventRacerRewriter::doVisit(BreakStatement *st) {
  st->target_ = st->target_->Accept(this);
  return st;
}

ReturnStatement *EventRacerRewriter::doVisit(ReturnStatement *st) {
  st->expression_ = st->expression_->Accept(this);
  return st;
}

WithStatement *EventRacerRewriter::doVisit(WithStatement *st) {
  st->expression_ = st->expression_->Accept(this);
  st->statement_ = st->statement_->Accept(this);
  return st;
}

CaseClause *EventRacerRewriter::doVisit(CaseClause *ex) {
  ex->label_ = ex->label_->Accept(this);
  ZoneList<Statement *> &ss = *ex->statements();
  for (int i = 0; i < ss.length(); ++i)
    ss[i] = ss[i]->Accept(this);
  return ex;
}

SwitchStatement *EventRacerRewriter::doVisit(SwitchStatement *st) {
  st->tag_ = st->tag_->Accept(this);
  ZoneList<CaseClause *> &cs = *st->cases();
  for (int i = 0; i < cs.length(); ++i)
    cs[i] = cs[i]->Accept(this);
  return st;
}

IfStatement *EventRacerRewriter::doVisit(IfStatement *st) {
  st->condition_ = st->condition_->Accept(this);
  st->then_statement_ = st->then_statement_->Accept(this);
  st->else_statement_ = st->else_statement_->Accept(this);
  return st;
}

TryCatchStatement *EventRacerRewriter::doVisit(TryCatchStatement *st) {
  st->try_block_ = st->try_block_->Accept(this);
  st->catch_block_ = st->catch_block_->Accept(this);
  return st;
}

TryFinallyStatement *EventRacerRewriter::doVisit(TryFinallyStatement *st) {
  st->try_block_ = st->try_block_->Accept(this);
  st->finally_block_ = st->finally_block_->Accept(this);
  return st;
}

ObjectLiteral* EventRacerRewriter::doVisit(ObjectLiteral *lit) {
  ZoneList<ObjectLiteral::Property *> &ps = *lit->properties();
  for (int i = 0; i < ps.length(); ++i)
    ps[i]->value_ = ps[i]->value_->Accept(this);
  return lit;
}

ArrayLiteral* EventRacerRewriter::doVisit(ArrayLiteral *lit) {
  ZoneList<Expression *> &vs = *lit->values();
  for (int i = 0; i < vs.length(); ++i)
    vs[i] = vs[i]->Accept(this);
  return lit;
}

Expression* EventRacerRewriter::doVisit(VariableProxy *var) {
  // TODO
  return var;
}

Expression* EventRacerRewriter::doVisit(Property *p) {
  // TODO
  p->obj_ = p->obj_->Accept(this);
  p->key_ = p->key_->Accept(this);
  return p;
}

Call* EventRacerRewriter::doVisit(Call *c) {
  c->expression_ = c->expression_->Accept(this);
  ZoneList<Expression *> &as = *c->arguments();
  for (int i = 0; i < as.length(); ++i)
    as[i] = as[i]->Accept(this);
  return c;
}

CallNew* EventRacerRewriter::doVisit(CallNew *c) {
  c->expression_ = c->expression_->Accept(this);
  ZoneList<Expression *> &as = *c->arguments();
  for (int i = 0; i < as.length(); ++i)
    as[i] = as[i]->Accept(this);
  return c;
}

CallRuntime* EventRacerRewriter::doVisit(CallRuntime *c) {
  ZoneList<Expression *> &as = *c->arguments();
  for (int i = 0; i < as.length(); ++i)
    as[i] = as[i]->Accept(this);
  return c;
}

UnaryOperation* EventRacerRewriter::doVisit(UnaryOperation *op) {
  op->expression_ = op->expression_->Accept(this);
  return op;
}

BinaryOperation* EventRacerRewriter::doVisit(BinaryOperation *op) {
  op->left_ = op->left_->Accept(this);
  op->right_ = op->right_->Accept(this);
  return op;
}

CountOperation* EventRacerRewriter::doVisit(CountOperation *op) {
  op->expression_ = op->expression_->Accept(this);
  return op;
}

CompareOperation* EventRacerRewriter::doVisit(CompareOperation *op) {
  op->left_ = op->left_->Accept(this);
  op->right_ = op->right_->Accept(this);
  return op;
}

Conditional* EventRacerRewriter::doVisit(Conditional *op) {
  op->condition_ = op->condition_->Accept(this);
  op->then_expression_ = op->then_expression_->Accept(this);
  op->else_expression_ = op->else_expression_->Accept(this);
  return op;
}

Assignment* EventRacerRewriter::doVisit(Assignment *op) {
  op->target_ = op->target_->Accept(this);
  op->value_ = op->value_->Accept(this);
  return op;
}

Yield* EventRacerRewriter::doVisit(Yield *op) {
  if (op->yield_kind() == Yield::SUSPEND || op->yield_kind() == Yield::FINAL)
    op->expression_ = op->expression_->Accept(this);
  return op;
}

Throw* EventRacerRewriter::doVisit(Throw *op) {
  op->exception_ = op->exception_->Accept(this);
  return op;
}

FunctionLiteral* EventRacerRewriter::doVisit(FunctionLiteral *lit) {
  ZoneList<Statement *> &body = *lit->body();
  for (int i = 0; i < body.length(); ++i)
    body[i] = body[i]->Accept(this);
  return lit;
}

} } // namespace v8::internal
