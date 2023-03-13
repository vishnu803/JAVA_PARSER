# Define the lexer
import ply.lex as lex

tokens = [
    'IMPORT',
    'CLASS',
    'ID',
    'PUBLIC',
    'PRIVATE',
    'PROTECTED',
    'STATIC',
    'VOID',
    'MAIN',
    'FOR',
    'WHILE',
    'IF',
    'ELSEIF',
    'ELSE',
    'TRUE',
    'FALSE',
    'EQ',
    'GT',
    'LT',
    'GE',
    'LE',
    'NE',
    'AND',
    'OR',
    'XOR',
    'INC',
    'DEC',
    'ADD',
    'SUB',
    'MUL',
    'DIV',
    'NUMBER',
    'CHAR_LIT',
    'STRING_LIT',
    'COMMENT',
    'BLOCK_COMMENT',
    'COLON',
    'INT',
    'FLOAT',
    'BOOLEAN',
    'CHAR',
    'LPAREN',
    'RPAREN',
    'LCPAREN',
    'RCPAREN',
    'ASSIGN',
    'STRING',
]
t_IMPORT = r'^import\s+([a-zA-Z_]\w*\.)+[a-zA-Z_]\w*;$'
t_CLASS = r'class'
t_PUBLIC = r'public'
t_PRIVATE = r'private'
t_INT = r'int'
t_FLOAT = r'float'
t_CHAR = r'char'
t_STRING = r'String'
t_BOOLEAN = r'boolean'
t_STATIC = r'static'
t_VOID = r'void'
t_MAIN = r'main'
t_FOR = r'for'
t_WHILE = r'while'
t_IF = r'if'
t_ELSEIF = r'else if'
t_ELSE = r'else'
t_TRUE = r'true'
t_FALSE = r'false'
t_EQ = r'=='
t_GT = r'>'
t_LT = r'<'
t_GE = r'>='
t_LE = r'<='
t_NE = r'!='
t_AND = r'&&'
t_OR = r'\|\|'
t_XOR = r'\^'
t_INC = r'\+\+'
t_DEC = r'\-\-'
t_ADD = r'\+'
t_SUB = r'\-'
t_MUL = r'\*'
t_DIV = r'/'
t_COLON = r';'
t_LPAREN = r'{'
t_RPAREN = r'}'
t_LCPAREN = r'\('
t_RCPAREN = r'\)'
t_ASSIGN = r'='
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value == 'public':
        t.type = 'PUBLIC'
    elif t.value == 'private':
        t.type = 'PRIVATE'
    elif t.value == 'protected':
        t.type = 'PROTECTED'
    elif t.value == 'static':
        t.type = 'STATIC'
    elif t.value == 'void':
        t.type = 'VOID'
    elif t.value == 'class':
        t.type = 'CLASS'
    elif t.value == 'int':
        t.type = 'INT'
    elif t.value == 'char':
        t.type = 'CHAR'
    elif t.value == 'String':
        t.type = 'STRING'
    elif t.value == 'boolean':
        t.type = 'BOOLEAN'
    elif t.value == 'float':
        t.type = 'FLOAT'
    elif t.value == 'import':
        t.type = 'IMPORT'
    elif t.value == 'if':
        t.type = 'IF'
    elif t.value == 'else if':
        t.type = 'ELSEIF'
    elif t.value == 'else':
        t.type = 'ELSE'
    elif t.value == 'for':
        t.type = 'FOR'
    elif t.value == 'while':
        t.type = 'WHILE'
    elif t.value == 'main':
        t.type = 'MAIN'
    elif t.value == 'true':
        t.type = 'TRUE'
    elif t.value == 'false':
        t.type = 'FALSE'
    return t

def t_NUMBER(t):
    r'[+-]?\d+(\.\d+)?'
    try:
        t.value = int(t.value)
    except ValueError:
        t.value = float(t.value)
    return t

t_CHAR_LIT = r'\'(?:\\.|[^\\\'])\''
t_STRING_LIT = r'"(?:\\.|[^\\"])*"'


t_ignore = ' \t\n'

# Define t_COMMENT token function
def t_COMMENT(t):
    r'//.*'
    pass

# Define t_BLOCK_COMMENT token function
def t_BLOCK_COMMENT(t):
    r'/\*[(.|\n)*]?\*/'
    pass

def t_error(t):
    print(f"Invalid token {t.value!r} on line {t.lineno}")
    t.lexer.skip(1)

lexer = lex.lex()


# Define the parser
import ply.yacc as yacc

#building binary tuple tree
#we are using (current node name, left child, right child)  

# program
def p_program(p):
    '''
    program : headers class
    '''

    p[0] = ('program', p[1], p[2])

# headers
def p_headers(p):
    '''
    headers : headers IMPORT
            |
    '''
    if len(p) == 3:
        p[0] = ('headerlist', p[1], p[2])
    else:
        p[0] = ()

# class
def p_class(p):
    '''
    class : access CLASS ID LPAREN class_body RPAREN
    '''
    p[0] = ('class', ('access', p[1], p[3]), p[5])

# access
def p_access(p):
    '''
    access : PUBLIC
           | PRIVATE
           | PROTECTED
           |
    '''
    p[0] = p[1]

# class_body
def p_class_body(p):
    '''
    class_body : main
    '''
    p[0] = p[1]

# main
def p_main(p):
    '''
    main : PUBLIC STATIC VOID MAIN LCPAREN RCPAREN LPAREN body RPAREN
    '''
    p[0] = p[8]

# body
def p_body(p):
    '''
    body : FOR LCPAREN expression COLON condition COLON expression RCPAREN LPAREN body RPAREN
        | WHILE LCPAREN condition RCPAREN LPAREN body RPAREN
        | IF LCPAREN condition RCPAREN LPAREN body RPAREN elif
        | expression COLON
        | body body
        |
    '''
    # print(len(p))
    if len(p) == 3:
        p[0] = ('body', p[1], p[2])
    elif len(p) == 8:
        p[0] = ('while', p[3], p[6])
    elif len(p) == 9:
        p[0] = ('if', p[8], ('cond-body', p[3], p[6]))
    elif len(p) == 12:
        p[0] = ('for', ('expressions', p[3], p[7]), ('cond-body', p[5], p[10]))
    else : 
        p[0] = ()

# elif
def p_elif(p):
    '''
    elif : ELSEIF LCPAREN condition RCPAREN LPAREN body RPAREN elif
        | ELSE LPAREN body RPAREN
        |
    '''
    if len(p) == 1 :
        p[0] = ()
    elif len(p) == 5 :
        p[0] = ('else', p[3])
    else:
        p[0] = ('else if', p[8], ('cond-body', p[3], p[6]))
        
# condition
def p_condition(p):
    '''
    condition : expr bicomp expr
    | condition bitwise condition
    | TRUE
    | FALSE
    | ID
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])

# bicomp
def p_bicomp(p):
    '''
    bicomp : EQ
           | GT
           | LT
           | GE
           | LE
           | NE
    '''
    p[0] = p[1]

# bitwise
def p_bitwise(p):
    '''
    bitwise : AND
            | OR
            | XOR
    '''
    p[0] = p[1]

# expression
def p_expression(p):
    '''
    expression : unary ID
    | ID unary
    | INT ID ASSIGN expr
    | FLOAT ID ASSIGN expr
    | CHAR ID ASSIGN CHAR_LIT
    | BOOLEAN ID ASSIGN condition
    | STRING ID ASSIGN STRING_LIT
    | ID ASSIGN expr
    | ID ASSIGN STRING_LIT
    | ID ASSIGN CHAR_LIT
    | INT ID
    | FLOAT ID
    | CHAR ID
    | BOOLEAN ID
    | STRING ID
    | ID ASSIGN condition
    |
    '''
    if len(p) == 5:
        p[0] = (p[1], ('=', p[2], p[4]))
    elif len(p) == 4:
        p[0] = ('=', p[1], p[3])
    elif len(p) == 3:
        p[0] = ('declaration', p[1], p[2])

def p_unary(p):
    '''
    unary : INC
          | DEC
    '''
    p[0] = p[1]

def p_expr(p):
    '''
    expr : expr ADD term
         | expr SUB term
         | term
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_term(p):
    '''
    term : term MUL factor
         | term DIV factor
         | factor
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_factor(p):
    '''
    factor : LCPAREN expr RCPAREN
           | value
    '''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_value(p):
    '''
    value : NUMBER
          | CHAR_LIT
          | STRING_LIT
          | ID
    '''
    p[0] = p[1]

def p_error(p):
    if p:
        print(f"Syntax error Unexpected token {p.value!r}")
    else:
        print("Syntax error: Unexpected end of input")

parser = yacc.yacc()

j_code = ''
with open('tests/loops_test.java', 'r') as f:
    j_code = f.read()
result = parser.parse(j_code, lexer=lexer)
import pprint
pprint.pprint(result)