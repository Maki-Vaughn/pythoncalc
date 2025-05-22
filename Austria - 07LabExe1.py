class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()

RESERVED_KEYWORDS = {
    'PROGRAM': Token('PROGRAM', 'PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'DIV': Token('DIV', 'DIV'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'REAL': Token('REAL', 'REAL'),
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END'),
}

class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('Invalid character')

    def advance(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()

    def _id(self):
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()
        token = RESERVED_KEYWORDS.get(result, Token('ID', result))
        return token

    def number(self):
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                self.current_char is not None and self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()

            token = Token('REAL_CONST', float(result))
        else:
            token = Token('INTEGER_CONST', int(result))

        return token

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ':':
                if self.peek() == '=':
                    self.advance()
                    self.advance()
                    return Token('ASSIGN', ':=')
                else:
                    self.advance()
                    return Token('COLON', ':')

            if self.current_char == ',':
                self.advance()
                return Token('COMMA', ',')

            if self.current_char == ';':
                self.advance()
                return Token('SEMI', ';')

            if self.current_char == '.':
                self.advance()
                return Token('DOT', '.')

            if self.current_char == '+':
                self.advance()
                return Token('PLUS', '+')

            if self.current_char == '-':
                self.advance()
                return Token('MINUS', '-')

            if self.current_char == '*':
                self.advance()
                return Token('MUL', '*')

            if self.current_char == '/':
                self.advance()
                return Token('DIV', '/')

            if self.current_char == '(':
                self.advance()
                return Token('LPAREN', '(')

            if self.current_char == ')':
                self.advance()
                return Token('RPAREN', ')')

            self.error()

        return Token('EOF', None)

class AST(object):
    pass

class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block

class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declaration = declarations
        self.compound_statement = compound_statement

class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        self.eat('PROGRAM')
        var_node = self.variable()
        prog_name = var_node.value
        self.eat('SEMI')
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat('DOT')
        return program_node

    def compound_statement(self):
        self.eat('BEGIN')
        nodes = self.statement_list()
        self.eat('END')
        return nodes

    def statement_list(self):
        node = self.statement()
        results = [node]
        while self.current_token.type == 'SEMI':
            self.eat('SEMI')
            results.append(self.statement())
        return results

    def statement(self):
        if self.current_token.type == 'BEGIN':
            node = self.compound_statement()
        elif self.current_token.type == 'ID':
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        left = self.variable()
        token = self.current_token
        self.eat('ASSIGN')
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self):
        node = Var(self.current_token)
        self.eat('ID')
        return node

    def empty(self):
        return None

    def factor(self):
        token = self.current_token
        if token.type == 'INTEGER_CONST':
            self.eat('INTEGER_CONST')
            return token.value
        elif token.type == 'REAL_CONST':
            self.eat('REAL_CONST')
            return token.value
        elif token.type == 'LPAREN':
            self.eat('LPAREN')
            node = self.expr()
            self.eat('RPAREN')
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        node = self.factor()
        while self.current_token.type in ('MUL', 'DIV'):
            if self.current_token.type == 'MUL':
                self.eat('MUL')
            elif self.current_token.type == 'DIV':
                self.eat('DIV')

            node = self.factor()
        return node

    def expr(self):
        node = self.term()
        while self.current_token.type in ('PLUS', 'MINUS'):
            if self.current_token.type == 'PLUS':
                self.eat('PLUS')
            elif self.current_token.type == 'MINUS':
                self.eat('MINUS')

            node = self.term()
        return node

    def block(self):
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self):
        declarations = []
        if self.current_token.type == 'VAR':
            self.eat('VAR')
            while self.current_token.type == 'ID':
                declarations.append(self.variable())
                self.eat('SEMI')
        return declarations

    def parse(self):
        return self.program()

class Interpreter:
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def interpret(self):
        tree = self.parser.parse()
        self.visit(tree)
        self.print_scope()

    def visit(self, node):
        if isinstance(node, Assign):
            self.GLOBAL_SCOPE[node.left.value] = self.visit(node.right)
        elif isinstance(node, Var):
            return self.GLOBAL_SCOPE[node.value]
        elif isinstance(node, list):
            for n in node:
                self.visit(n)

    def print_scope(self):
        for var, value in self.GLOBAL_SCOPE.items():
            print(f"{var} = {value}")

def main():
    text = input("Enter the program: ")

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    interpreter.interpret()

if __name__ == '__main__':
    main()
