#ifndef V8_EVENT_RACER_REWRITER_H_
#define V8_EVENT_RACER_REWRITER_H_

#include "src/ast.h"

namespace v8 {
namespace internal {

#define TMP_AST_NODE_LIST(V)                    \
  V(VariableDeclaration)                        \
  V(FunctionDeclaration)                        \
  V(ModuleDeclaration)                          \
  V(ImportDeclaration)                          \
  V(ExportDeclaration)                          \
                                                \
  V(ModuleLiteral)                              \
  V(ModuleVariable)                             \
  V(ModulePath)                                 \
  V(ModuleUrl)                                  \
                                                \
  V(Block)                                      \
  V(ModuleStatement)                            \
  V(ExpressionStatement)                        \
  V(EmptyStatement)                             \
  V(IfStatement)                                \
  V(ContinueStatement)                          \
  V(BreakStatement)                             \
  V(ReturnStatement)                            \
  V(WithStatement)                              \
  V(SwitchStatement)                            \
  V(DoWhileStatement)                           \
  V(WhileStatement)                             \
  V(ForStatement)                               \
  V(ForInStatement)                             \
  V(ForOfStatement)                             \
  V(TryCatchStatement)                          \
  V(TryFinallyStatement)                        \
  V(DebuggerStatement)                          \
                                                \
  V(FunctionLiteral)                            \
  V(NativeFunctionLiteral)                      \
  V(Conditional)                                \
  V(VariableProxy)                              \
  V(Literal)                                    \
  V(RegExpLiteral)                              \
  V(ObjectLiteral)                              \
  V(ArrayLiteral)                               \
  V(Assignment)                                 \
  V(Yield)                                      \
  V(Throw)                                      \
  V(Property)                                   \
  V(Call)                                       \
  V(CallNew)                                    \
  V(CallRuntime)                                \
  V(UnaryOperation)                             \
  V(CountOperation)                             \
  V(BinaryOperation)                            \
  V(CompareOperation)                           \
  V(ThisFunction)                               \
  V(CaseClause)

class EventRacerRewriter : public AstRewriter {
public:
  EventRacerRewriter(Zone *z, AstValueFactory *f)
    : value_factory_(f),
      factory_(z, f) {
    InitializeAstRewriter(z);
  }

#define DEF_VISIT(type) \
   virtual type* Visit##type(type *nd) V8_FINAL V8_OVERRIDE { return nd; }
   TMP_AST_NODE_LIST(DEF_VISIT)
#undef DEF_VISIT

  DEFINE_AST_REWRITER_SUBCLASS_MEMBERS();

private:
  AstValueFactory *value_factory_;
  AstNodeFactory<AstConstructionVisitor> factory_;
};

} }  // namespace v8::internal

#endif // V8_EVENT_RACER_REWRITER_H_
