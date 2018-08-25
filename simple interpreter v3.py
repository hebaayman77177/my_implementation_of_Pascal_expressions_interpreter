'''interpreter second attempt
----------lexer
----------parser
----------interpreter
---------------and first for calculator'''


#just make things easy
#to make things bounded
#so the error is less

(INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, ID, ASSIGN,
 BEGIN, END, SEMI, DOT, EOF) = (
    'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', '(', ')', 'ID', 'ASSIGN',
    'BEGIN', 'END', 'SEMI', 'DOT', 'EOF'
)





class Token :
    def __init__(self,type,value):
        self.type=type
        self.value=value




    def __str__(self):

        return "Token({type} ,{value})".format(
            type=self.type
            ,value=self.value)

    def __repr__(self):
        return self.__str__()


RESERVED_KEYWORDS = {
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END')
}

class Lexer:
    def __init__(self,text):
        self.text=text
        self.pos=0
        self.current_char=self.text[self.pos]


    def error(self):
        raise Exception('Invalid character')


    def advance(self):
        self.pos+=1
        if self.pos < len(self.text):
            self.current_char=self.text[self.pos]
        else:
            self.current_char= None

    def peek(self):
        pos=self.pos+1
        if pos<len(self.text):
            return self.text[pos]

        else:
            return None
        



    def skip_white_space(self):
        if self.current_char.isspace() and not None:
            self.advance()

    def __id(self):
        result=''
        while self.current_char is not None and self.current_char.isalnum():
            result=result+self.current_char
            self.advance()
        return result

        


    def integer(self):
        result =''
        while self.current_char != None and self.current_char.isdigit():
            result=result+self.current_char
            self.advance()


        return int(result)









    def get_next_token(self):
        while (self.current_char is not None):



            if self.current_char.isspace():
                self.skip_white_space()
                

            elif self.current_char.isdigit():
                result=self.integer()
                return Token(INTEGER,result)

            elif self.current_char.isalpha():
                result=self.__id()
                token = RESERVED_KEYWORDS.get(result, Token(ID, result))
    
                return token

            elif self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')

            elif self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            elif self.current_char == '.':
                self.advance()
                return Token(DOT, '.')

            
            elif self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')

            elif self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')

            elif self.current_char == '*':
                self.advance()
                return Token(MUL, '*')

            elif self.current_char == '/':
                self.advance()
                return Token(DIV, '/')

            elif self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')

            elif self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')


            else:
                self.error()


        return Token(EOF,None)

##########

# every node type inhirit from it
#for polymarphism


##class AST:
##    pass

class Compound:
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []

    def append (self,node):
        return self.children.append(node)

    def __str__(self):
        return ' compound node'

    def __repr__(self):
        return self.__str__


class Assign:#like BINOP ,why we put it?
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op

        self.right = right

    def __str__(self):
        return ' assignment node'

    def __repr__(self):
        return self.__str__
    
class NoOp:
    pass




class Var: #######like num ,why we put it?
    """The Var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        self.value = token.value

    def __str__(self):
        return ' var node'

    def __repr__(self):
        return self.__str__
    
class Num:
    def __init__(self,token):
        self.token=token
        self.value=token.value
        

class BinOp:
    def __init__(self,left,op,right):
        self.left=left
        self.op=op
        self.right=right
        

class UnaryOp:
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Parser:

    
    def __init__(self,lexer):
        self.lexer=lexer
        self.current_token=self.lexer.get_next_token()


    def error(self):
        raise Exception('Invalid syntax')


    def eat(self,type):
        if self.current_token.type==type:
            self.current_token=self.lexer.get_next_token()

        else :
            self.error()

    

    def factor(self):
        node=self.current_token
        if self.current_token.type== INTEGER:
            self.eat(INTEGER)
            return Num(node)

        if self.current_token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(node, self.factor())
            return node
        
        elif self.current_token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(node, self.factor())
            return node

        elif self.current_token.type== LPAREN:
            self.eat(LPAREN)
            node=self.expr()
            self.eat(RPAREN)

            return node


        elif self.current_token.type == ID:
            node = self.variable()
            return node


        else:
            self.error()





    def term(self):
        node=self.factor()
    
        while self.current_token.type in (MUL,DIV):
            if self.current_token.type==MUL:
                self.eat(MUL)
                node=BinOp(node,Token(MUL,'*'), self.factor())

            elif self.current_token.type==DIV:
                self.eat(DIV)
                node=BinOp(node,Token(dIV,'/'), self.factor())
                                        



								        
        return node


                
    def expr(self):
        node=self.term()
        
        while self.current_token.type in(PLUS,MINUS):
            if self.current_token.type == PLUS:
                self.eat(PLUS)
                node=BinOp(node,Token(PLUS,'+'), self.factor())
            elif self.current_token.type == MINUS:
                self.eat(MINUS)
                node=BinOp(node,Token(MINUS,'-'), self.factor()) 
        
        return node

    def program(self):
        """program : compound_statement DOT"""
        node = self.compound_statement()
        self.eat(DOT)
        return node


    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)

        root = Compound()
        for node in nodes:    
            root.append(node)

        return root 

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results =[node]
        
        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())

##        if self.current_token.type == ID:
##            self.error()

        return results
      


    def statement(self):
        """
        statement : compound_statement
                  | assignment_statement
                  | empty
        """
        if self.current_token.type == BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == ID:
            node = self.assignment_statement()
        else:#error from empty func as i think
            node = self.empty()
            
        return node


    def assignment_statement(self):
        """
       assignment_statement : variable ASSIGN expr
    """
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node



    def variable(self):
        """
        variable : ID
        """
        node = Var(self.current_token)
        self.eat(ID)
        return node


    def empty(self):
        """An empty production"""
        return NoOp()

    def parse(self):
        return self.program()




        

        

####################
#interpreter



class NodeVisitor:
    def visit(self,node):
        method_name='visit_' + type(node).__name__
        visitor=getattr(self,method_name,self.generic_visit)
        return visitor(node)

    def generic_visit(self,node):
        raise Exception('no visit_{} method'.format(type(node).__name__))



class Interpreter(NodeVisitor):

    def __init__(self,parser):
        self.parser=parser
        self.GLOBAL_SCOPE={}

    #base case
    def visit_Num(self,node):
        return node.value

    
    def visit_BinOp(self,node):
        if node.op.type==PLUS:
            return self.visit(node.left)+self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass
    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)
    def visit_Var(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val
    def interpret(self):
        tree=self.parser.parse()
        return self.visit(tree)




















    



        
#i = Interpreter(Parser(Lexer(' BEGIN BEGIN x:=5;y:=x+1 END; z:=7 END.')))
#print(i.interpret())

#print(i.GLOBAL_SCOPE)
      
        


##
##p=Parser(Lexer(' BEGIN x:=5;x=x+1 END.'))
##n=p.parse()   
##print(n.children)











        
        
        
        



            
        
        

 








                

















        




    
            



    

    












        
        



    
















