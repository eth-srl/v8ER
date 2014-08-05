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
  AstRewriterImpl(Zone *z, AstValueFactory *f)
    : value_factory_(f),
      factory_(z, f) {
    InitializeAstRewriter(z);
  }

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

private:
  AstValueFactory *value_factory_;
  AstNodeFactory<AstConstructionVisitor> factory_;
};

typedef AstRewriterImpl<EventRacerRewriterTag> EventRacerRewriter;

} }  // namespace v8::internal

#endif // V8_EVENT_RACER_REWRITER_H_
