# Generated from antlr/Kotlisp.g4 by ANTLR 4.13.1
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,34,129,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,1,0,5,0,26,8,0,10,
        0,12,0,29,9,0,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,2,1,2,1,2,1,
        2,1,2,3,2,45,8,2,1,2,5,2,48,8,2,10,2,12,2,51,9,2,1,2,1,2,1,3,1,3,
        1,3,1,3,1,3,1,3,3,3,61,8,3,1,3,1,3,1,3,1,3,1,3,1,3,4,3,69,8,3,11,
        3,12,3,70,3,3,73,8,3,1,4,1,4,1,4,4,4,78,8,4,11,4,12,4,79,1,4,4,4,
        83,8,4,11,4,12,4,84,1,4,4,4,88,8,4,11,4,12,4,89,3,4,92,8,4,1,4,1,
        4,1,5,1,5,1,5,1,5,1,6,1,6,1,6,1,6,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,
        8,1,8,1,8,1,8,1,8,1,9,1,9,1,9,1,9,1,9,1,10,1,10,1,10,1,11,1,11,1,
        11,1,11,1,11,1,11,0,0,12,0,2,4,6,8,10,12,14,16,18,20,22,0,3,1,0,
        13,26,2,0,29,29,32,32,1,0,10,12,141,0,27,1,0,0,0,2,30,1,0,0,0,4,
        36,1,0,0,0,6,72,1,0,0,0,8,74,1,0,0,0,10,95,1,0,0,0,12,99,1,0,0,0,
        14,103,1,0,0,0,16,110,1,0,0,0,18,115,1,0,0,0,20,120,1,0,0,0,22,123,
        1,0,0,0,24,26,3,2,1,0,25,24,1,0,0,0,26,29,1,0,0,0,27,25,1,0,0,0,
        27,28,1,0,0,0,28,1,1,0,0,0,29,27,1,0,0,0,30,31,5,27,0,0,31,32,5,
        1,0,0,32,33,5,32,0,0,33,34,3,4,2,0,34,35,5,28,0,0,35,3,1,0,0,0,36,
        44,5,27,0,0,37,45,3,12,6,0,38,45,3,14,7,0,39,45,3,16,8,0,40,45,3,
        10,5,0,41,45,3,20,10,0,42,45,3,6,3,0,43,45,3,22,11,0,44,37,1,0,0,
        0,44,38,1,0,0,0,44,39,1,0,0,0,44,40,1,0,0,0,44,41,1,0,0,0,44,42,
        1,0,0,0,44,43,1,0,0,0,44,45,1,0,0,0,45,49,1,0,0,0,46,48,3,4,2,0,
        47,46,1,0,0,0,48,51,1,0,0,0,49,47,1,0,0,0,49,50,1,0,0,0,50,52,1,
        0,0,0,51,49,1,0,0,0,52,53,5,28,0,0,53,5,1,0,0,0,54,61,5,31,0,0,55,
        61,3,8,4,0,56,61,5,29,0,0,57,61,5,32,0,0,58,61,5,30,0,0,59,61,3,
        18,9,0,60,54,1,0,0,0,60,55,1,0,0,0,60,56,1,0,0,0,60,57,1,0,0,0,60,
        58,1,0,0,0,60,59,1,0,0,0,61,73,1,0,0,0,62,68,7,0,0,0,63,69,3,8,4,
        0,64,69,5,29,0,0,65,69,5,32,0,0,66,69,5,30,0,0,67,69,3,18,9,0,68,
        63,1,0,0,0,68,64,1,0,0,0,68,65,1,0,0,0,68,66,1,0,0,0,68,67,1,0,0,
        0,69,70,1,0,0,0,70,68,1,0,0,0,70,71,1,0,0,0,71,73,1,0,0,0,72,60,
        1,0,0,0,72,62,1,0,0,0,73,7,1,0,0,0,74,75,5,2,0,0,75,91,5,27,0,0,
        76,78,5,29,0,0,77,76,1,0,0,0,78,79,1,0,0,0,79,77,1,0,0,0,79,80,1,
        0,0,0,80,92,1,0,0,0,81,83,5,31,0,0,82,81,1,0,0,0,83,84,1,0,0,0,84,
        82,1,0,0,0,84,85,1,0,0,0,85,92,1,0,0,0,86,88,5,30,0,0,87,86,1,0,
        0,0,88,89,1,0,0,0,89,87,1,0,0,0,89,90,1,0,0,0,90,92,1,0,0,0,91,77,
        1,0,0,0,91,82,1,0,0,0,91,87,1,0,0,0,92,93,1,0,0,0,93,94,5,28,0,0,
        94,9,1,0,0,0,95,96,5,32,0,0,96,97,5,27,0,0,97,98,5,28,0,0,98,11,
        1,0,0,0,99,100,5,3,0,0,100,101,5,32,0,0,101,102,3,4,2,0,102,13,1,
        0,0,0,103,104,5,4,0,0,104,105,5,27,0,0,105,106,5,32,0,0,106,107,
        7,1,0,0,107,108,5,28,0,0,108,109,3,4,2,0,109,15,1,0,0,0,110,111,
        5,5,0,0,111,112,3,4,2,0,112,113,3,4,2,0,113,114,3,4,2,0,114,17,1,
        0,0,0,115,116,5,32,0,0,116,117,5,6,0,0,117,118,7,1,0,0,118,119,5,
        7,0,0,119,19,1,0,0,0,120,121,5,8,0,0,121,122,3,4,2,0,122,21,1,0,
        0,0,123,124,5,9,0,0,124,125,5,27,0,0,125,126,7,2,0,0,126,127,5,28,
        0,0,127,23,1,0,0,0,11,27,44,49,60,68,70,72,79,84,89,91
    ]

class KotlispParser ( Parser ):

    grammarFileName = "Kotlisp.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'fun'", "'list'", "'setq'", "'dotimes'", 
                     "'if'", "'['", "']'", "'print'", "'read-line'", "'int'", 
                     "'str'", "'bool'", "'+'", "'-'", "'*'", "'/'", "'^'", 
                     "'&'", "'%'", "'|'", "'='", "'!='", "'>'", "'>='", 
                     "'<'", "'<='", "'('", "')'" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "PLUS", "MINUS", "MULT", "DIV", "XOR", 
                      "AND", "MOD", "OR", "EQUALS", "NOT_EQUALS", "GREATER", 
                      "GREATER_OR_EQUALS", "LESS", "LESS_OR_EQUALS", "OP", 
                      "CP", "NUMBER", "BOOLEAN", "STRING", "NAME", "WHITESPACE", 
                      "COMMENT" ]

    RULE_kotlisp = 0
    RULE_fun = 1
    RULE_s_expression = 2
    RULE_math_expression = 3
    RULE_list_ = 4
    RULE_fun_call = 5
    RULE_variable = 6
    RULE_loop = 7
    RULE_conditional = 8
    RULE_index = 9
    RULE_print_ = 10
    RULE_read_line = 11

    ruleNames =  [ "kotlisp", "fun", "s_expression", "math_expression", 
                   "list_", "fun_call", "variable", "loop", "conditional", 
                   "index", "print_", "read_line" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    T__4=5
    T__5=6
    T__6=7
    T__7=8
    T__8=9
    T__9=10
    T__10=11
    T__11=12
    PLUS=13
    MINUS=14
    MULT=15
    DIV=16
    XOR=17
    AND=18
    MOD=19
    OR=20
    EQUALS=21
    NOT_EQUALS=22
    GREATER=23
    GREATER_OR_EQUALS=24
    LESS=25
    LESS_OR_EQUALS=26
    OP=27
    CP=28
    NUMBER=29
    BOOLEAN=30
    STRING=31
    NAME=32
    WHITESPACE=33
    COMMENT=34

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.1")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class KotlispContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def fun(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(KotlispParser.FunContext)
            else:
                return self.getTypedRuleContext(KotlispParser.FunContext,i)


        def getRuleIndex(self):
            return KotlispParser.RULE_kotlisp

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterKotlisp" ):
                listener.enterKotlisp(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitKotlisp" ):
                listener.exitKotlisp(self)




    def kotlisp(self):

        localctx = KotlispParser.KotlispContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_kotlisp)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 27
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==27:
                self.state = 24
                self.fun()
                self.state = 29
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FunContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def NAME(self):
            return self.getToken(KotlispParser.NAME, 0)

        def s_expression(self):
            return self.getTypedRuleContext(KotlispParser.S_expressionContext,0)


        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_fun

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterFun" ):
                listener.enterFun(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitFun" ):
                listener.exitFun(self)




    def fun(self):

        localctx = KotlispParser.FunContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_fun)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 30
            self.match(KotlispParser.OP)
            self.state = 31
            self.match(KotlispParser.T__0)
            self.state = 32
            self.match(KotlispParser.NAME)
            self.state = 33
            self.s_expression()
            self.state = 34
            self.match(KotlispParser.CP)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class S_expressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def variable(self):
            return self.getTypedRuleContext(KotlispParser.VariableContext,0)


        def loop(self):
            return self.getTypedRuleContext(KotlispParser.LoopContext,0)


        def conditional(self):
            return self.getTypedRuleContext(KotlispParser.ConditionalContext,0)


        def fun_call(self):
            return self.getTypedRuleContext(KotlispParser.Fun_callContext,0)


        def print_(self):
            return self.getTypedRuleContext(KotlispParser.Print_Context,0)


        def math_expression(self):
            return self.getTypedRuleContext(KotlispParser.Math_expressionContext,0)


        def read_line(self):
            return self.getTypedRuleContext(KotlispParser.Read_lineContext,0)


        def s_expression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(KotlispParser.S_expressionContext)
            else:
                return self.getTypedRuleContext(KotlispParser.S_expressionContext,i)


        def getRuleIndex(self):
            return KotlispParser.RULE_s_expression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterS_expression" ):
                listener.enterS_expression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitS_expression" ):
                listener.exitS_expression(self)




    def s_expression(self):

        localctx = KotlispParser.S_expressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_s_expression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 36
            self.match(KotlispParser.OP)
            self.state = 44
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,1,self._ctx)
            if la_ == 1:
                self.state = 37
                self.variable()

            elif la_ == 2:
                self.state = 38
                self.loop()

            elif la_ == 3:
                self.state = 39
                self.conditional()

            elif la_ == 4:
                self.state = 40
                self.fun_call()

            elif la_ == 5:
                self.state = 41
                self.print_()

            elif la_ == 6:
                self.state = 42
                self.math_expression()

            elif la_ == 7:
                self.state = 43
                self.read_line()


            self.state = 49
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==27:
                self.state = 46
                self.s_expression()
                self.state = 51
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 52
            self.match(KotlispParser.CP)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Math_expressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(KotlispParser.STRING, 0)

        def list_(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(KotlispParser.List_Context)
            else:
                return self.getTypedRuleContext(KotlispParser.List_Context,i)


        def NUMBER(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.NUMBER)
            else:
                return self.getToken(KotlispParser.NUMBER, i)

        def NAME(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.NAME)
            else:
                return self.getToken(KotlispParser.NAME, i)

        def BOOLEAN(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.BOOLEAN)
            else:
                return self.getToken(KotlispParser.BOOLEAN, i)

        def index(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(KotlispParser.IndexContext)
            else:
                return self.getTypedRuleContext(KotlispParser.IndexContext,i)


        def PLUS(self):
            return self.getToken(KotlispParser.PLUS, 0)

        def MINUS(self):
            return self.getToken(KotlispParser.MINUS, 0)

        def MULT(self):
            return self.getToken(KotlispParser.MULT, 0)

        def DIV(self):
            return self.getToken(KotlispParser.DIV, 0)

        def XOR(self):
            return self.getToken(KotlispParser.XOR, 0)

        def AND(self):
            return self.getToken(KotlispParser.AND, 0)

        def OR(self):
            return self.getToken(KotlispParser.OR, 0)

        def EQUALS(self):
            return self.getToken(KotlispParser.EQUALS, 0)

        def NOT_EQUALS(self):
            return self.getToken(KotlispParser.NOT_EQUALS, 0)

        def GREATER(self):
            return self.getToken(KotlispParser.GREATER, 0)

        def GREATER_OR_EQUALS(self):
            return self.getToken(KotlispParser.GREATER_OR_EQUALS, 0)

        def LESS(self):
            return self.getToken(KotlispParser.LESS, 0)

        def LESS_OR_EQUALS(self):
            return self.getToken(KotlispParser.LESS_OR_EQUALS, 0)

        def MOD(self):
            return self.getToken(KotlispParser.MOD, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_math_expression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMath_expression" ):
                listener.enterMath_expression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMath_expression" ):
                listener.exitMath_expression(self)




    def math_expression(self):

        localctx = KotlispParser.Math_expressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_math_expression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 72
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [2, 29, 30, 31, 32]:
                self.state = 60
                self._errHandler.sync(self)
                la_ = self._interp.adaptivePredict(self._input,3,self._ctx)
                if la_ == 1:
                    self.state = 54
                    self.match(KotlispParser.STRING)
                    pass

                elif la_ == 2:
                    self.state = 55
                    self.list_()
                    pass

                elif la_ == 3:
                    self.state = 56
                    self.match(KotlispParser.NUMBER)
                    pass

                elif la_ == 4:
                    self.state = 57
                    self.match(KotlispParser.NAME)
                    pass

                elif la_ == 5:
                    self.state = 58
                    self.match(KotlispParser.BOOLEAN)
                    pass

                elif la_ == 6:
                    self.state = 59
                    self.index()
                    pass


                pass
            elif token in [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]:
                self.state = 62
                _la = self._input.LA(1)
                if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 134209536) != 0)):
                    self._errHandler.recoverInline(self)
                else:
                    self._errHandler.reportMatch(self)
                    self.consume()
                self.state = 68 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while True:
                    self.state = 68
                    self._errHandler.sync(self)
                    la_ = self._interp.adaptivePredict(self._input,4,self._ctx)
                    if la_ == 1:
                        self.state = 63
                        self.list_()
                        pass

                    elif la_ == 2:
                        self.state = 64
                        self.match(KotlispParser.NUMBER)
                        pass

                    elif la_ == 3:
                        self.state = 65
                        self.match(KotlispParser.NAME)
                        pass

                    elif la_ == 4:
                        self.state = 66
                        self.match(KotlispParser.BOOLEAN)
                        pass

                    elif la_ == 5:
                        self.state = 67
                        self.index()
                        pass


                    self.state = 70 
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 5905580036) != 0)):
                        break

                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class List_Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def NUMBER(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.NUMBER)
            else:
                return self.getToken(KotlispParser.NUMBER, i)

        def STRING(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.STRING)
            else:
                return self.getToken(KotlispParser.STRING, i)

        def BOOLEAN(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.BOOLEAN)
            else:
                return self.getToken(KotlispParser.BOOLEAN, i)

        def getRuleIndex(self):
            return KotlispParser.RULE_list_

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterList_" ):
                listener.enterList_(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitList_" ):
                listener.exitList_(self)




    def list_(self):

        localctx = KotlispParser.List_Context(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_list_)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 74
            self.match(KotlispParser.T__1)
            self.state = 75
            self.match(KotlispParser.OP)
            self.state = 91
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [29]:
                self.state = 77 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while True:
                    self.state = 76
                    self.match(KotlispParser.NUMBER)
                    self.state = 79 
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if not (_la==29):
                        break

                pass
            elif token in [31]:
                self.state = 82 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while True:
                    self.state = 81
                    self.match(KotlispParser.STRING)
                    self.state = 84 
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if not (_la==31):
                        break

                pass
            elif token in [30]:
                self.state = 87 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while True:
                    self.state = 86
                    self.match(KotlispParser.BOOLEAN)
                    self.state = 89 
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if not (_la==30):
                        break

                pass
            else:
                raise NoViableAltException(self)

            self.state = 93
            self.match(KotlispParser.CP)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Fun_callContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NAME(self):
            return self.getToken(KotlispParser.NAME, 0)

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_fun_call

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterFun_call" ):
                listener.enterFun_call(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitFun_call" ):
                listener.exitFun_call(self)




    def fun_call(self):

        localctx = KotlispParser.Fun_callContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_fun_call)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 95
            self.match(KotlispParser.NAME)
            self.state = 96
            self.match(KotlispParser.OP)
            self.state = 97
            self.match(KotlispParser.CP)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class VariableContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NAME(self):
            return self.getToken(KotlispParser.NAME, 0)

        def s_expression(self):
            return self.getTypedRuleContext(KotlispParser.S_expressionContext,0)


        def getRuleIndex(self):
            return KotlispParser.RULE_variable

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterVariable" ):
                listener.enterVariable(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitVariable" ):
                listener.exitVariable(self)




    def variable(self):

        localctx = KotlispParser.VariableContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_variable)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 99
            self.match(KotlispParser.T__2)
            self.state = 100
            self.match(KotlispParser.NAME)
            self.state = 101
            self.s_expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LoopContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def NAME(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.NAME)
            else:
                return self.getToken(KotlispParser.NAME, i)

        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def s_expression(self):
            return self.getTypedRuleContext(KotlispParser.S_expressionContext,0)


        def NUMBER(self):
            return self.getToken(KotlispParser.NUMBER, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_loop

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLoop" ):
                listener.enterLoop(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLoop" ):
                listener.exitLoop(self)




    def loop(self):

        localctx = KotlispParser.LoopContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_loop)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 103
            self.match(KotlispParser.T__3)
            self.state = 104
            self.match(KotlispParser.OP)
            self.state = 105
            self.match(KotlispParser.NAME)
            self.state = 106
            _la = self._input.LA(1)
            if not(_la==29 or _la==32):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
            self.state = 107
            self.match(KotlispParser.CP)
            self.state = 108
            self.s_expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ConditionalContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def s_expression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(KotlispParser.S_expressionContext)
            else:
                return self.getTypedRuleContext(KotlispParser.S_expressionContext,i)


        def getRuleIndex(self):
            return KotlispParser.RULE_conditional

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterConditional" ):
                listener.enterConditional(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitConditional" ):
                listener.exitConditional(self)




    def conditional(self):

        localctx = KotlispParser.ConditionalContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_conditional)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 110
            self.match(KotlispParser.T__4)
            self.state = 111
            self.s_expression()
            self.state = 112
            self.s_expression()
            self.state = 113
            self.s_expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class IndexContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NAME(self, i:int=None):
            if i is None:
                return self.getTokens(KotlispParser.NAME)
            else:
                return self.getToken(KotlispParser.NAME, i)

        def NUMBER(self):
            return self.getToken(KotlispParser.NUMBER, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_index

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterIndex" ):
                listener.enterIndex(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitIndex" ):
                listener.exitIndex(self)




    def index(self):

        localctx = KotlispParser.IndexContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_index)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 115
            self.match(KotlispParser.NAME)
            self.state = 116
            self.match(KotlispParser.T__5)
            self.state = 117
            _la = self._input.LA(1)
            if not(_la==29 or _la==32):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
            self.state = 118
            self.match(KotlispParser.T__6)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Print_Context(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def s_expression(self):
            return self.getTypedRuleContext(KotlispParser.S_expressionContext,0)


        def getRuleIndex(self):
            return KotlispParser.RULE_print_

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterPrint_" ):
                listener.enterPrint_(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitPrint_" ):
                listener.exitPrint_(self)




    def print_(self):

        localctx = KotlispParser.Print_Context(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_print_)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 120
            self.match(KotlispParser.T__7)
            self.state = 121
            self.s_expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class Read_lineContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def OP(self):
            return self.getToken(KotlispParser.OP, 0)

        def CP(self):
            return self.getToken(KotlispParser.CP, 0)

        def getRuleIndex(self):
            return KotlispParser.RULE_read_line

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRead_line" ):
                listener.enterRead_line(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRead_line" ):
                listener.exitRead_line(self)




    def read_line(self):

        localctx = KotlispParser.Read_lineContext(self, self._ctx, self.state)
        self.enterRule(localctx, 22, self.RULE_read_line)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 123
            self.match(KotlispParser.T__8)
            self.state = 124
            self.match(KotlispParser.OP)
            self.state = 125
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 7168) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
            self.state = 126
            self.match(KotlispParser.CP)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





