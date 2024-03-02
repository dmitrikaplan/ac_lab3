# Generated from antlr/Kotlisp.g4 by ANTLR 4.13.1
from antlr4 import *

if "." in __name__:
    from .KotlispParser import KotlispParser
else:
    from KotlispParser import KotlispParser


# This class defines a complete listener for a parse tree produced by KotlispParser.
class KotlispListener(ParseTreeListener):
    # Enter a parse tree produced by KotlispParser#kotlisp.
    def enterKotlisp(self, ctx: KotlispParser.KotlispContext):
        pass

    # Exit a parse tree produced by KotlispParser#kotlisp.
    def exitKotlisp(self, ctx: KotlispParser.KotlispContext):
        pass

    # Enter a parse tree produced by KotlispParser#fun.
    def enterFun(self, ctx: KotlispParser.FunContext):
        pass

    # Exit a parse tree produced by KotlispParser#fun.
    def exitFun(self, ctx: KotlispParser.FunContext):
        pass

    # Enter a parse tree produced by KotlispParser#s_expression.
    def enterS_expression(self, ctx: KotlispParser.S_expressionContext):
        pass

    # Exit a parse tree produced by KotlispParser#s_expression.
    def exitS_expression(self, ctx: KotlispParser.S_expressionContext):
        pass

    # Enter a parse tree produced by KotlispParser#math_expression.
    def enterMath_expression(self, ctx: KotlispParser.Math_expressionContext):
        pass

    # Exit a parse tree produced by KotlispParser#math_expression.
    def exitMath_expression(self, ctx: KotlispParser.Math_expressionContext):
        pass

    # Enter a parse tree produced by KotlispParser#list_.
    def enterList_(self, ctx: KotlispParser.List_Context):
        pass

    # Exit a parse tree produced by KotlispParser#list_.
    def exitList_(self, ctx: KotlispParser.List_Context):
        pass

    # Enter a parse tree produced by KotlispParser#fun_call.
    def enterFun_call(self, ctx: KotlispParser.Fun_callContext):
        pass

    # Exit a parse tree produced by KotlispParser#fun_call.
    def exitFun_call(self, ctx: KotlispParser.Fun_callContext):
        pass

    # Enter a parse tree produced by KotlispParser#variable.
    def enterVariable(self, ctx: KotlispParser.VariableContext):
        pass

    # Exit a parse tree produced by KotlispParser#variable.
    def exitVariable(self, ctx: KotlispParser.VariableContext):
        pass

    # Enter a parse tree produced by KotlispParser#loop.
    def enterLoop(self, ctx: KotlispParser.LoopContext):
        pass

    # Exit a parse tree produced by KotlispParser#loop.
    def exitLoop(self, ctx: KotlispParser.LoopContext):
        pass

    # Enter a parse tree produced by KotlispParser#conditional.
    def enterConditional(self, ctx: KotlispParser.ConditionalContext):
        pass

    # Exit a parse tree produced by KotlispParser#conditional.
    def exitConditional(self, ctx: KotlispParser.ConditionalContext):
        pass

    # Enter a parse tree produced by KotlispParser#index.
    def enterIndex(self, ctx: KotlispParser.IndexContext):
        pass

    # Exit a parse tree produced by KotlispParser#index.
    def exitIndex(self, ctx: KotlispParser.IndexContext):
        pass

    # Enter a parse tree produced by KotlispParser#print_.
    def enterPrint_(self, ctx: KotlispParser.Print_Context):
        pass

    # Exit a parse tree produced by KotlispParser#print_.
    def exitPrint_(self, ctx: KotlispParser.Print_Context):
        pass

    # Enter a parse tree produced by KotlispParser#read_line.
    def enterRead_line(self, ctx: KotlispParser.Read_lineContext):
        pass

    # Exit a parse tree produced by KotlispParser#read_line.
    def exitRead_line(self, ctx: KotlispParser.Read_lineContext):
        pass


del KotlispParser
