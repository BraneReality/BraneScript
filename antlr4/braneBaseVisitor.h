
// Generated from /home/wirewhiz/CLionProjects/BraneScript/antlr4/brane.g4 by ANTLR 4.13.1

#pragma once


#include "antlr4-runtime.h"
#include "braneVisitor.h"


/**
 * This class provides an empty implementation of braneVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the available methods.
 */
class  braneBaseVisitor : public braneVisitor {
public:

  virtual std::any visitModules(braneParser::ModulesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitLink(braneParser::LinkContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitLinkList(braneParser::LinkListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTags(braneParser::TagsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitModule(braneParser::ModuleContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitModuleComponent(braneParser::ModuleComponentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitGlobal(braneParser::GlobalContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTemplateDefArgument(braneParser::TemplateDefArgumentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTemplateDef(braneParser::TemplateDefContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTemplateTypeArg(braneParser::TemplateTypeArgContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTemplateExprArg(braneParser::TemplateExprArgContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPackExpansionArg(braneParser::PackExpansionArgContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitTemplateArgs(braneParser::TemplateArgsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitScopedID(braneParser::ScopedIDContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitType(braneParser::TypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitDeclaration(braneParser::DeclarationContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitArgumentListItem(braneParser::ArgumentListItemContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitArgumentList(braneParser::ArgumentListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitArgumentPackItem(braneParser::ArgumentPackItemContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitArgumentPack(braneParser::ArgumentPackContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitBracketOpr(braneParser::BracketOprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitFunctionSig(braneParser::FunctionSigContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitFunctionStub(braneParser::FunctionStubContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitFunction(braneParser::FunctionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitCapturedVar(braneParser::CapturedVarContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitVarCapture(braneParser::VarCaptureContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitStructDef(braneParser::StructDefContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMemberFunctionStub(braneParser::MemberFunctionStubContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMemberFunction(braneParser::MemberFunctionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMemberVariable(braneParser::MemberVariableContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitDecl(braneParser::DeclContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitVoidExpression(braneParser::VoidExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitFor(braneParser::ForContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitAddsub(braneParser::AddsubContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPreDec(braneParser::PreDecContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitRefAssignment(braneParser::RefAssignmentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitWhile(braneParser::WhileContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitSwitch(braneParser::SwitchContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitEmpty(braneParser::EmptyContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitCast(braneParser::CastContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitNot(braneParser::NotContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitParen(braneParser::ParenContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitLambda(braneParser::LambdaContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMagicFunctionCall(braneParser::MagicFunctionCallContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitScope(braneParser::ScopeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitId(braneParser::IdContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitIf(braneParser::IfContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitConstChar(braneParser::ConstCharContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMemberAccess(braneParser::MemberAccessContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitComparison(braneParser::ComparisonContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitAssignment(braneParser::AssignmentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMatch(braneParser::MatchContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitConstInt(braneParser::ConstIntContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitConstBool(braneParser::ConstBoolContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPostDec(braneParser::PostDecContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMuldiv(braneParser::MuldivContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPreInc(braneParser::PreIncContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitIndexAccess(braneParser::IndexAccessContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitPostInc(braneParser::PostIncContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitConstString(braneParser::ConstStringContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitFunctionCall(braneParser::FunctionCallContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitConstFloat(braneParser::ConstFloatContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitLogic(braneParser::LogicContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitReturn(braneParser::ReturnContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitSwitchCase(braneParser::SwitchCaseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitMatchCase(braneParser::MatchCaseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitSizeOfType(braneParser::SizeOfTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitSizeOfPack(braneParser::SizeOfPackContext *ctx) override {
    return visitChildren(ctx);
  }


};

