"""
COMP3109 Assignment 2 - Task 3
Caitlin Mangan, Jacqueline Leykam, Renee Noble

parser.py: Interfaces with yacc (via ply) to parse the input file and create
data structures representing the code's structure.

"""

import ply.yacc as yacc
import lexer
import program

tokens = lexer.tokens


class ParseError(Exception):

    """
    Represents a syntax error in the input program.
    """

    def __init__(self, token, pos):
        self.token = token
        self.pos = pos

    def __str__(self):
        return "Something to do with a %s around character %s" % (
            self.token, self.pos)


# For each of of the following functions, ply passes a sliceable object, p,
# as the argument. p[0] defaults to None, whilst p[1:] is set to the parsed
# structure (eg. '(', 'ld', 'r1', 5, ')'). The value set for p[0] propogates up
# the parse tree, allowing us to easily create an AST (or other structure) from
# the input.


def p_program(p):
    '''program : LPAREN functions RPAREN'''
    p[0] = program.Program(p[2])


def p_functions(p):
    '''functions : empty
                 | function functions'''
    p[0] = [p[1]]
    if len(p[1:]) == 2 and p[2] != [None]:
        p[0] += p[2]


def p_function(p):
    '''function : LPAREN ID arguments blocks RPAREN'''
    p[0] = program.Function(p[2], p[3], p[4])


def p_arguments(p):
    '''arguments : LPAREN id_list RPAREN'''
    p[0] = p[2]


def p_id_list(p):
    '''id_list : empty
               | ID id_list'''
    p[0] = [p[1]]
    if len(p[1:]) == 2 and p[2] != [None]:
        p[0] += p[2]


def p_blocks(p):
    '''blocks : LPAREN NUM instructions RPAREN
              | LPAREN NUM instructions RPAREN blocks'''
    p[0] = [program.Block(p[2], p[3])]
    if len(p[1:]) == 5:
        p[0] += p[5]


def p_instructions(p):
    '''instructions : instruction
                    | instruction instructions'''
    p[0] = [p[1]]
    if len(p[1:]) == 2:
        p[0] += p[2]


def p_instruction(p):
    '''instruction : LPAREN LOADCONST REG NUM RPAREN
                   | LPAREN LOADINST REG ID RPAREN
                   | LPAREN STOREINST ID REG RPAREN
                   | LPAREN ADD REG REG REG RPAREN
                   | LPAREN SUB REG REG REG RPAREN
                   | LPAREN MUL REG REG REG RPAREN
                   | LPAREN DIV REG REG REG RPAREN
                   | LPAREN LT REG REG REG RPAREN
                   | LPAREN GT REG REG REG RPAREN
                   | LPAREN CMP REG REG REG RPAREN
                   | LPAREN BRANCH REG NUM NUM RPAREN
                   | LPAREN RETURN REG RPAREN
                   | LPAREN CALL REG ID reg_list RPAREN'''
    p[0] = program.Instruction(p[2], p[3:-1])


def p_reg_list(p):
    '''reg_list : empty
                | REG reg_list'''
    p[0] = [i for i in p[1:] if i is not None and i != []]


def p_empty(p):
    '''empty :'''
    pass


def p_error(p):
    raise ParseError(p.type, p.lexpos)

# Create the parser from the given rules.
yacc.yacc()


def parse(source):
    return yacc.parse(source)
