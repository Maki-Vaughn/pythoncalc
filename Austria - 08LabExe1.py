class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        return f"Token({self.type}, {repr(self.value)})"

    def __repr__(self):
        return self.__str__()

RESERVED_KEYWORDS = {
    'PROGRAM': Token('PROGRAM', 'PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'DIV': Token('INTEGER_DIV', 'DIV'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'REAL': Token('REAL', 'REAL'),
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END'),
}

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def advance(self):
        self.pos += 1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

    def peek(self):
        peek_pos = self.pos + 1
        return self.text[peek_pos] if peek_pos < len(self.text) else None

    def skip_whitespace(self):
        while self.current_char and self.current_char.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()

    def _id(self):
        result = ''
        while self.current_char and self.current_char.isalnum():
            result += self.current_char
            self.advance()
        return RESERVED_KEYWORDS.get(result.upper(), Token('ID', result))

    def number(self):
        result = ''
        while self.current_char and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        if self.current_char == '.':
            result += self.current_char
            self.advance()
            while self.current_char and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            return Token('REAL_CONST', float(result))
        return Token('INTEGER_CONST', int(result))

    def get_next_token(self):
        while self.current_char:
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
            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token('ASSIGN', ':=')
            if self.current_char == ':':
                self.advance()
                return Token('COLON', ':')
            if self.current_char == ';':
                self.advance()
                return Token('SEMI', ';')
            if self.current_char == ',':
                self.advance()
                return Token('COMMA', ',')
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
                return Token('FLOAT_DIV', '/')
            if self.current_char == '(':
                self.advance()
                return Token('LPAREN', '(')
            if self.current_char == ')':
                self.advance()
                return Token('RPAREN', ')')
            raise Exception(f'Invalid character: {self.current_char}')
        return Token('EOF', None)

# AST Nodes
class AST: pass
class BinOp(AST):
    def __init__(self, left, op, right): self.left, self.op, self.right = left, op, right
class Num(AST):
    def __init__(self, token): self.token = token; self.value = token.value
class UnaryOp(AST):
    def __init__(self, op, expr): self.op, self.expr = op, expr
class Compound(AST):
    def __init__(self): self.children = []
class Assign(AST):
    def __init__(self, left, op, right): self.left, self.token, self.right = left, op, right
class Var(AST):
    def __init__(self, token): self.token = token; self.value = token.value
class NoOp(AST): pass
class Program(AST):
    def __init__(self, name, block): self.name, self.block = name, block
class Block(AST):
    def __init__(self, declarations, compound_statement): self.declarations, self.compound_statement = declarations, compound_statement
class VarDecl(AST):
    def __init__(self, var_node, type_node): self.var_node, self.type_node = var_node, type_node
class Type(AST):
    def __init__(self, token): self.token, self.value = token, token.value

# Parser
class Parser:
    def __init__(self, lexer): self.lexer = lexer; self.current_token = self.lexer.get_next_token()
    def error(self): raise Exception('Syntax error')
    def eat(self, token_type):
        if self.current_token.type == token_type: self.current_token = self.lexer.get_next_token()
        else: self.error()
    def program(self):
        self.eat('PROGRAM')
        var_node = self.variable()
        prog_name = var_node.value
        self.eat('SEMI')
        block_node = self.block()
        self.eat('DOT')
        return Program(prog_name, block_node)
    def block(self): return Block(self.declarations(), self.compound_statement())
    def declarations(self):
        declarations = []
        if self.current_token.type == 'VAR':
            self.eat('VAR')
            while self.current_token.type == 'ID':
                var_nodes = [Var(self.current_token)]
                self.eat('ID')
                while self.current_token.type == 'COMMA':
                    self.eat('COMMA')
                    var_nodes.append(Var(self.current_token))
                    self.eat('ID')
                self.eat('COLON')
                type_node = Type(self.current_token)
                self.eat(self.current_token.type)
                self.eat('SEMI')
                for var_node in var_nodes:
                    declarations.append(VarDecl(var_node, type_node))
        return declarations
    def compound_statement(self):
        self.eat('BEGIN')
        nodes = self.statement_list()
        self.eat('END')
        compound = Compound()
        compound.children = nodes
        return compound
    def statement_list(self):
        node = self.statement()
        results = [node]
        while self.current_token.type == 'SEMI':
            self.eat('SEMI')
            results.append(self.statement())
        return results
    def statement(self):
        if self.current_token.type == 'BEGIN': return self.compound_statement()
        elif self.current_token.type == 'ID': return self.assignment_statement()
        return NoOp()
    def assignment_statement(self):
        left = self.variable()
        token = self.current_token
        self.eat('ASSIGN')
        right = self.expr()
        return Assign(left, token, right)
    def variable(self):
        node = Var(self.current_token)
        self.eat('ID')
        return node
    def expr(self):
        node = self.term()
        while self.current_token.type in ('PLUS', 'MINUS'):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(node, token, self.term())
        return node
    def term(self):
        node = self.factor()
        while self.current_token.type in ('MUL', 'INTEGER_DIV', 'FLOAT_DIV'):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(node, token, self.factor())
        return node
    def factor(self):
        token = self.current_token
        if token.type in ('PLUS', 'MINUS'):
            self.eat(token.type)
            return UnaryOp(token, self.factor())
        elif token.type in ('INTEGER_CONST', 'REAL_CONST'):
            self.eat(token.type)
            return Num(token)
        elif token.type == 'LPAREN':
            self.eat('LPAREN')
            node = self.expr()
            self.eat('RPAREN')
            return node
        return self.variable()
    def parse(self): return self.program()

class Symbol:
    def __init__(self, name, type=None): self.name, self.type = name, type
    def __str__(self): return f"<{self.name}:{self.type}>"
    def __repr__(self): return self.__str__()

class SymbolTable:
    def __init__(self): self._symbols = {}
    def define(self, symbol): self._symbols[symbol.name] = symbol
    def lookup(self, name): return self._symbols.get(name)
    def __str__(self): return "\n".join(["Symbols:"] + [str(s) for s in self._symbols.values()])
    def __repr__(self): return self.__str__()

class NodeVisitor:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)
    def generic_visit(self, node): raise Exception(f'No visit_{type(node).__name__} method')

class SymbolTableBuilder(NodeVisitor):
    def __init__(self): self.symtab = SymbolTable()
    def visit_Program(self, node): self.visit(node.block)
    def visit_Block(self, node):
        for decl in node.declarations: self.visit(decl)
        self.visit(node.compound_statement)
    def visit_VarDecl(self, node):
        type_symbol = Symbol(node.type_node.value)
        var_symbol = Symbol(node.var_node.value, type_symbol.name)
        if self.symtab.lookup(var_symbol.name):
            raise Exception(f"Duplicate identifier '{var_symbol.name}'")
        self.symtab.define(var_symbol)
    def visit_Compound(self, node):
        for child in node.children: self.visit(child)
    def visit_Assign(self, node):
        if self.symtab.lookup(node.left.value) is None:
            raise NameError(f"Semantic error: Identifier not found '{node.left.value}'")
        self.visit(node.right)
    def visit_Var(self, node):
        if self.symtab.lookup(node.value) is None:
            raise NameError(f"Semantic error: Identifier not found '{node.value}'")
    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)
    def visit_Num(self, node): pass
    def visit_UnaryOp(self, node): self.visit(node.expr)
    def visit_NoOp(self, node): pass

class Interpreter(NodeVisitor):
    def __init__(self, tree): self.tree = tree; self.GLOBAL_MEMORY = {}
    def interpret(self): self.visit(self.tree); return self.GLOBAL_MEMORY
    def visit_Program(self, node): self.visit(node.block)
    def visit_Block(self, node):
        for decl in node.declarations: self.visit(decl)
        self.visit(node.compound_statement)
    def visit_VarDecl(self, node): pass
    def visit_Type(self, node): pass
    def visit_Compound(self, node):
        for child in node.children: self.visit(child)
    def visit_Assign(self, node):
        self.GLOBAL_MEMORY[node.left.value] = self.visit(node.right)
    def visit_Var(self, node):
        val = self.GLOBAL_MEMORY.get(node.value)
        if val is None: raise NameError(f"Variable '{node.value}' is not defined")
        return val
    def visit_NoOp(self, node): pass
    def visit_BinOp(self, node):
        if node.op.type == 'PLUS': return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == 'MINUS': return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == 'MUL': return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == 'INTEGER_DIV': return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == 'FLOAT_DIV': return self.visit(node.left) / self.visit(node.right)
    def visit_Num(self, node): return node.value
    def visit_UnaryOp(self, node):
        if node.op.type == 'PLUS': return +self.visit(node.expr)
        elif node.op.type == 'MINUS': return -self.visit(node.expr)

def main():
    text = '''
    PROGRAM Part11;
    VAR
       number : INTEGER;
       a, b   : INTEGER;
       y      : REAL;

    BEGIN {Part11}
       number := 2;
       a := number ;
       b := 10 * a + 10 * number DIV 4;
       y := 20 / 7 + 3.14
    END.  {Part11}
    '''

    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    symtab_builder = SymbolTableBuilder()
    symtab_builder.visit(tree)

    print("\nSymbol Table contents:")
    print(symtab_builder.symtab)

    interpreter = Interpreter(tree)
    memory = interpreter.interpret()

    print("\nRun-time GLOBAL_MEMORY contents:")
    for k in sorted(memory):
        print(f"{k} = {memory[k]}")

if __name__ == '__main__':
    main()
