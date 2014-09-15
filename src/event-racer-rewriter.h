#ifndef V8_EVENT_RACER_REWRITER_H_
#define V8_EVENT_RACER_REWRITER_H_

#include "src/ast.h"
#include "src/scopes.h"

namespace v8 {
namespace internal {

// Only concrete AST classes
#define INSTRUMENTED_AST_NODE_LIST(V)           \
  V(FunctionDeclaration)                        \
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
  V(BinaryOperation)                            \
  V(CompareOperation)                           \
  V(Conditional)                                \
  V(Yield)                                      \
  V(Throw)                                      \
  V(FunctionLiteral)

#define UNMODIFIED_AST_NODE_LIST(V)             \
  V(VariableDeclaration)                        \
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

#define INSTRUMENTATION_FUNCTION_LIST(V)        \
  V(ER_read)                                    \
  V(ER_write)                                   \
  V(ER_readProp)                                \
  V(ER_writeProp)                               \
  V(ER_readPropIdx)                             \
  V(ER_writePropIdx)                            \
  V(ER_writePropIdxStrict)                      \
  V(ER_preIncProp)                              \
  V(ER_preIncPropStrict)                        \
  V(ER_preDecProp)                              \
  V(ER_preDecPropStrict)                        \
  V(ER_postIncProp)                             \
  V(ER_postIncPropStrict)                       \
  V(ER_postDecProp)                             \
  V(ER_postDecPropStrict)                       \
  V(ER_delete)                                  \
  V(ER_deleteProp)                              \
  V(ER_deletePropIdx)                           \
  V(ER_deletePropIdxStrict)

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
  virtual Expression* doVisit(CountOperation *) V8_FINAL V8_OVERRIDE;
  virtual Expression* doVisit(UnaryOperation *) V8_FINAL V8_OVERRIDE;
  virtual Expression* doVisit(Assignment *) V8_FINAL V8_OVERRIDE;

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

  struct AstNodeIdAllocationScope {
    AstNodeIdAllocationScope(AstRewriterImpl<EventRacerRewriterTag> *w)
      : rewriter(w) {
      prev = w->id_alloc_scope_;
      saved_ast_node_id = w->ast_node_id();
      w->set_ast_node_id(BailoutId::FirstUsable().ToInt());
    }

    AstNodeIdAllocationScope(AstRewriterImpl<EventRacerRewriterTag> *w,
                             FunctionLiteral *fn)
      : rewriter(w) {
      prev = w->id_alloc_scope_;
      saved_ast_node_id = w->ast_node_id();
      w->set_ast_node_id(fn->next_ast_node_id());
    }

    ~AstNodeIdAllocationScope() {
      rewriter->set_ast_node_id(saved_ast_node_id);
      rewriter->id_alloc_scope_ = prev;
    }

    int saved_ast_node_id;
    AstRewriterImpl<EventRacerRewriterTag> *rewriter;
    AstNodeIdAllocationScope *prev;
  };

  // Hack around protected members of Scope, that we need to call.
  class ScopeHack : public Scope {
  public:
    ScopeHack(Scope* outer_scope, AstValueFactory* value_factory, Zone* zone)
      : Scope(outer_scope, FUNCTION_SCOPE, value_factory, zone) {

      Initialize();
      ForceEagerCompilation();
      outer_scope_calls_sloppy_eval_ =
        (outer_scope_->calls_sloppy_eval()
         || outer_scope_->outer_scope_calls_sloppy_eval());
    }

    void AllocateStackSlot(Variable* var) {
      Scope::AllocateStackSlot(var);
    }
  };

  bool is_potentially_shared(const VariableProxy *vp) const {
    Variable *var = vp->var();
    return var == NULL || !var->IsStackAllocated();
  }

  ContextScope *context() const { return current_context_; }

  CompilationInfo *info_;
  ContextScope *current_context_;
  AstNodeIdAllocationScope *id_alloc_scope_;

  AstNodeFactory<AstNullVisitor> factory_;
  const AstRawString *o_string_, *k_string_, *v_string_;
  ZoneList<const AstRawString *> *arg_names_;

#define FN(x) x,
  enum InstrumentationFunction {
    INSTRUMENTATION_FUNCTION_LIST(FN)
    FN_MAX
  };
#undef FN
  Variable *instr_fn_[FN_MAX];
  VariableProxy *fn_proxy(enum InstrumentationFunction);

  ScopeHack *NewScope(Scope* outer);
  void ensure_arg_names(int n);
  int ast_node_id() const;
  void set_ast_node_id(int);

  bool is_literal_key(const Expression *) const;
  Literal *duplicate_key(const Literal *);

  FunctionLiteral *make_fn(Scope *scope, ZoneList<Statement *> *body,
                           int param_count, int ast_node_id, int pos);
};

typedef AstRewriterImpl<EventRacerRewriterTag> EventRacerRewriter;

class AstSlotCounter : public AstVisitor {
public:
  AstSlotCounter() : state_(&FunctionState::guard) {}

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

    static FunctionState guard;
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
