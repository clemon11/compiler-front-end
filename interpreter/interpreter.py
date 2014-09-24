#!/usr/bin/env python2
"""
COMP3109 Assignment 2 - Task 3
Caitlin Mangan, Jacqueline Leykam, Renee Noble

interpreter.py: Handles the interpreter's command line interface.

"""

import sys
import parser
import program


def run(prog, args):
    """
    Executes the given program.Program, passing the given arguments to the main
    function.
    """
    # Create a stack frame for the main function.
    frame = program.Frame(prog, prog.getFunction("main"))

    # Fill out the main function's arguments.
    if len(args) != len(frame.func.args):
        raise program.InterpreterError("Expected %s args, got %s"
                                       % (len(frame.func.args), len(args)))
    for i, arg in enumerate(args):
        try:
            args[i] = int(arg)
        except ValueError:
            raise program.InterpreterError("Arguments must be integers.")
    for i, arg in enumerate(frame.func.args):
        frame.variables[arg] = args[i]

    # Execute the program and print the result.
    print frame.fetchExecute()


def usage():
    print "Usage: interpreter.py SOURCEFILE [ARGS]..."
    print """
    Execute the intermediary code contained within SOURCEFILE, passing ARGS to
    the program's main method."""
    sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        usage()
    try:
        with open(sys.argv[1], "rU") as f:
            source = ''.join(f.readlines())
            try:
                prog = parser.parse(source)
                if prog is not None:
                    run(prog, sys.argv[2:])
            except parser.ParseError as err:
                print "Syntax error:", err
                try:
                    # Make a small effort to indicate where the problem is.
                    print
                    print source[:err.pos]+'\033[41m'+source[err.pos]+'\033[49m'+source[err.pos+1:]
                except IndexError:
                    pass
            except program.InterpreterError as err:
                print "Runtime error:", err
            except RuntimeError:
                print "Runtime error: Max recursion depth exceeded"
    except IOError as err:
        print "Failed to open", sys.argv[1] + ":", err.strerror
