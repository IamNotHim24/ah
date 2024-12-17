################
#DIGITS
################
DIGITS = '0123456789'

################
#ERROR
################
class Error:
    def __init__(self,posStart,posEnd,errorName,detail):
        self.posStart = posStart
        self.posEnd = posEnd
        self.errorName = errorName
        self.detail = detail

    def as_string(self):
        result = f'{self.errorName}: {self.detail}'
        result += f'\nat:[{self.posStart.idx} to {self.posEnd.idx}]'
        return result
    
class IllegalCharacter(Error):
    def __init__(self, posStart, posEnd, detail):
        super().__init__(posStart, posEnd, 'Illegal Character', detail)

class InvalidSyntax(Error):
    def __init__(self, posStart, posEnd,  detail):
        super().__init__(posStart, posEnd, 'invalid syntax', detail)



################
#POSITION
################
class Position:
    def __init__(self,idx,ftxt):
        self.idx = idx
        self.ftxt = ftxt
    
    def advance(self):
        self.idx += 1
        
        return self
    
    def copy(self):
        return Position(self.idx,self.ftxt)



###############
#TOKENS
###############
TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'LPAREN'
TT_EOF = 'EOF'

class Token:
    def __init__(self,type_,value = None,posStart=None,posEnd=None):
        self.type = type_
        self.value = value
        if posStart:
            self.posStart = posStart.copy()
            self.posEnd = posStart.copy()
            self.posEnd.advance()

        if posEnd:
            self.posEnd = posEnd

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

##############
#Lexer
##############
class Lexer:
    def __init__(self,text):
        self.text = text
        self.pos = Position(-1,text)
        self.currChar = None
        self.advance()

    def advance(self):
        self.pos.advance()
        self.currChar = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None


    def make_tokens(self):
        tokens = []

        while self.currChar != None:
            if self.currChar in ' \t':
                self.advance()
                continue
            if self.currChar == '+':
                tokens.append(Token(TT_PLUS,posStart=self.pos))
                self.advance()
            elif self.currChar == '-':
                tokens.append(Token(TT_MINUS,posStart=self.pos))
                self.advance()
            elif self.currChar == '*':
                tokens.append(Token(TT_MUL,posStart=self.pos))
                self.advance()
            elif self.currChar == '/':
                tokens.append(Token(TT_DIV,posStart=self.pos))
                self.advance()
            elif self.currChar == '(':
                tokens.append(Token(TT_LPAREN,posStart=self.pos))
                self.advance()
            elif self.currChar == ')':
                tokens.append(Token(TT_RPAREN,posStart=self.pos))
                self.advance()
            elif self.currChar in DIGITS:
                tokens.append(self.make_number())
            else:
                posStart = self.pos.copy()
                char = self.currChar
                self.advance()
                return [],IllegalCharacter(posStart,self.pos,"'"+char+"'")
        tokens.append(Token(TT_EOF,posStart=self.pos))
        return tokens,None
        
    def make_number(self):
        posStart = self.pos.copy()
        num_str = ''
        dot_cnt = 0

        while self.currChar!= None and self.currChar in DIGITS + '.':
            if self.currChar == '.':
                if dot_cnt == 1: break
                dot_cnt += 1
                num_str += '.'
            else:
                num_str += self.currChar
            self.advance()
        if dot_cnt == 0:
            return Token(TT_INT,int(num_str),posStart,self.pos)
        else:
            return Token(TT_FLOAT,float(num_str),posStart,self.pos)

##############
#NODES
##############
class NumberNode:
    def __init__(self,tok):
        self.tok = tok
    def __repr__(self):
        return f'{self.tok}'
    
class BinOpNode:
    def __init__(self,left_node,op_tok,right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

    def __repr__(self):
        return f'({self.left_node},{self.op_tok},{self.right_node})' 
    
class UnaryOpNode:
    def __init__(self,op_tok,node):
        self.op_tok = op_tok
        self.node = node
    
    def __repr__(self):
        return f'({self.op_tok,self.node})'
    
##############
#ParseResult
##############

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self,res):
        if isinstance(res,ParseResult):
            if res.error: self.error = res.error
            return res.node
        return res
    
    def success(self,node):
        self.node = node
        return self
    
    def failure(self,error):
        self.error = error
        return self

##############
#Parser
##############
class Parser:
    def __init__(self,tokens):
        self.tokens = tokens
        self.tok_idx = -1 
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx<len(self.tokens):
            self.currTok = self.tokens[self.tok_idx]
        return self.currTok
    
    def bin_op(self,func,ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res
        while self.currTok.type in ops:
            op_tok = self.currTok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left,op_tok,right)

        return res.success(left)
    
    def factor(self):
        res = ParseResult()
        tok = self.currTok
        if tok.type in (TT_PLUS,TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok,factor))

        elif tok.type in (TT_INT,TT_FLOAT):
            res.register(self.advance())
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.currTok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
        else:
            return res.failure(InvalidSyntax(tok.posStart,tok.posEnd,"Expected an ')'"))
        return res.failure(InvalidSyntax(tok.posStart,tok.posEnd,'Expected an int or float'))
    
    
    def term(self):
        return self.bin_op(self.factor,(TT_MUL,TT_DIV))
    
    def expr(self):
        return self.bin_op(self.term,(TT_PLUS,TT_MINUS))

    def parse(self):
        res = self.expr()
        if not res.error and self.currTok.type!=TT_EOF:
            return res.failure(InvalidSyntax(self.currTok.posStart,self.currTok.posEnd,"Expected '+',  '-', '*' or '/'"))
        return res


def run(text):
    lexer = Lexer(text)
    tokens,error = lexer.make_tokens()

    parser = Parser(tokens)
    ast  = parser.parse()
    return ast.node,ast.error