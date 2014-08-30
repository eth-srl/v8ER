#ifndef V8_EVENT_RACER_REWRITER_H_
#define V8_EVENT_RACER_REWRITER_H_

#include "src/ast.h"

namespace v8 {
namespace internal {

// Only concrete AST classes
#define INSTRUMENTED_AST_NODE_LIST(V)           \
  V(Block)                                      \
  V(ExpressionStatement)                        \
  V(DoWhileStatement)                           \
  V(WhileStatement)                             \
  V(ForStatement)                               \
  V(ForInStatement)                             \
  V(ForOfStatement)                             \
  V(ReturnStatement)                            \
  V(WithStatement)                              \
  V(CaseClause)                                 \
  V(SwitchStatement)                            \
  V(IfStatement)                                \
  V(TryCatchStatement)                          \
  V(TryFinallyStatement)                        \
  V(ObjectLiteral)                              \
  V(ArrayLiteral)                               \
  V(Call)                                       \
  V(CallNew)                                    \
  V(CallRuntime)                                \
  V(UnaryOperation)                             \
  V(BinaryOperation)                            \
  V(CountOperation)                             \
  V(CompareOperation)                           \
  V(Conditional)                                \
  V(Assignment)                                 \
  V(Yield)                                      \
  V(Throw)                                      \
  V(FunctionLiteral)

#define UNMODIFIED_AST_NODE_LIST(V)             \
  V(VariableDeclaration)                        \
  V(FunctionDeclaration)                        \
  V(ModuleDeclaration)                          \
  V(ImportDeclaration)                          \
  V(ExportDeclaration)                          \
  V(ModuleLiteral)                              \
  V(ModuleVariable)                             \
  V(ModulePath)                                 \
  V(ModuleUrl)                                  \
  V(ModuleStatement)                            \
  V(DebuggerStatement)                          \
  V(ContinueStatement)                          \
  V(BreakStatement)                             \
  V(EmptyStatement)                             \
  V(Literal)                                    \
  V(RegExpLiteral)                              \
  V(NativeFunctionLiteral)                      \
  V(ThisFunction)

struct EventRacerRewriterTag {};

template<>
class AstRewriterImpl<EventRacerRewriterTag> : public AstRewriter {
public:
  AstRewriterImpl(CompilationInfo *info);

#define DEF_VISIT(type) \
  virtual type* doVisit(type *nd) V8_FINAL V8_OVERRIDE;
  INSTRUMENTED_AST_NODE_LIST(DEF_VISIT)
#undef DEF_VISIT

#define DEF_VISIT(type) \
  virtual type* doVisit(type *nd) V8_FINAL V8_OVERRIDE { return nd; }
  UNMODIFIED_AST_NODE_LIST(DEF_VISIT)
#undef DEF_VISIT

  virtual Expression* doVisit(VariableProxy *) V8_FINAL V8_OVERRIDE;
  virtual Expression* doVisit(Property *) V8_FINAL V8_OVERRIDE;

  void scope_analysis_complete() { post_scope_analysis_ = true; }

private:
  struct ContextScope {
    ContextScope(AstRewriterImpl<EventRacerRewriterTag> *w,
                 Scope *s = NULL)
      : rewriter(w) {
      prev = w->current_context_;
      if (s)
        scope = s;
      else if (prev)
        scope = prev->scope;
      else
        scope = NULL;
      w->current_context_ = this;
    }

    ~ContextScope() {
      rewriter->current_context_ = prev;
    }

    AstRewriterImpl<EventRacerRewriterTag> *rewriter;
    Scope *scope;
    ContextScope *prev;
  };

  bool is_potentially_shared(const VariableProxy *vp) const {
    DCHECK(post_scope_analysis_);
    if (vp->do_not_instrument())
      return false;
    Variable *var = vp->var();
    return var == NULL || !var->IsStackAllocated();
  }

  ContextScope *context() const { return current_context_; }

  CompilationInfo *info_;
  bool post_scope_analysis_;
  ContextScope *current_context_;

  AstNodeFactory<AstNullVisitor> factory_;
  VariableProxy *ER_read_proxy_;
  VariableProxy *ER_readProp_proxy_;
  const AstRawString *o_string_, *k_string_;
  ZoneList<const AstRawString *> *arg_names_;

  Scope *NewScope(Scope* outer, ScopeType type);
  void ensure_arg_names(int n);
};

typedef AstRewriterImpl<EventRacerRewriterTag> EventRacerRewriter;

class AstSlotCounter : public AstVisitor {
public:
  AstSlotCounter() : state_(NULL) {}

#define DECLARE_VISIT(type) virtual void Visit##type(type* node);
  AST_NODE_LIST(DECLARE_VISIT)
#undef DECLARE_VISIT


  void add_node();
  void add_materialized_literal(MaterializedLiteral *);
  void add_feedback_slot(FeedbackSlotInterface *);

private:
  struct FunctionState {
    FunctionState()
      : prev(NULL),
        materialized_literal_count(JSFunction::kLiteralsPrefixSize),
        feedback_slot_count(0),
        node_count(0) {}

    FunctionState *prev;
    int materialized_literal_count;
    int feedback_slot_count;
    int node_count;
  };

  void begin_function (FunctionState *st) {
    st->prev = state_;
    state_ = st;
  }
  void end_function() {
    state_ = state_->prev;
  }

  FunctionState *state_;

  DEFINE_AST_VISITOR_SUBCLASS_MEMBERS();
};


} }  // namespace v8::internal

#endif // V8_EVENT_RACER_REWRITER_H_
