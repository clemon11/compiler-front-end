"""
COMP3109 Assignment 2 - Task 3
Caitlin Mangan, Jacqueline Leykam, Renee Noble

lexer.py: Defines the tokens to be used for lexing the source file.

"""

import ply.lex as lex

tokens = ["LPAREN", "RPAREN", "NUM", "ID", "REG"]

t_LPAREN = r'\('
t_RPAREN = r'\)'

# The lexer will mistake these keywords for ids as it gives tokens with a
# longer regex higher precedence. We get around this by treating these
# 'reserved words' differently.
reserved = {
    r'lc': "LOADCONST",
    r'ld': "LOADINST",
    r'st': "STOREINST",
    r'add': "ADD",
    r'sub': "SUB",
    r'mul': "MUL",
    r'div': "DIV",
    r'lt': "LT",
    r'gt': "GT",
    r'eq': "CMP",
    r'br': "BRANCH",
    r'ret': "RETURN",
    r'call': "CALL"
}
tokens += reserved.values()

# NOTE: The assignment sheet seems to interchangeably refer to cmp and eq. For
# simplicity's sake we accept both of them.
reserved[r'cmp'] = "CMP"

# More precedence tweaking... regexes in functions are used in the order in
# in which they are defined. We need to define t_REG before t_ID so that
# registers have greater precedence than ids or else the id regex will eat them
# all.


def t_REG(tok):
    r'r[1-9][0-9]*'
    return tok


def t_ID(tok):
    r'[a-zA-Z][a-zA-Z0-9]*'
    if tok.value in reserved:
        tok.type = reserved[tok.value]
    return tok


def t_NUM(tok):
    r'-?[0-9]+'
    tok.value = int(tok.value)
    return tok

# Ignore whitespace.
t_ignore = ' \n'

# Skip over any illegal characters.


def t_error(tok):
    print "Warning: illegal character '%s'" % tok.value[0]
    tok.lexer.skip(1)

# Construct the lexer from the given rules.
lex.lex()
