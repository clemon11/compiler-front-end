"""
COMP3109 Assignment 2 - Task 3
Caitlin Mangan, Jacqueline Leykam, Renee Noble

program.py: Interprets the input file using the constructs created by the
parser.

"""


class InterpreterError(Exception):

    """
    Represents an error in the logic of the input program.
    """

    def __init__(self, message):
        self.message = message

    def __str__(self):
        return self.message


class UndefinedRegister(InterpreterError):

    def __init__(self, register):
        InterpreterError.__init__(
            self,
            "Read from undefined register %s" %
            register)


class UndefinedVariable(InterpreterError):

    def __init__(self, variable):
        InterpreterError.__init__(
            self,
            "Read from undefined variable %s" %
            variable)


class UndefinedFunction(InterpreterError):

    def __init__(self, function):
        InterpreterError.__init__(self, "Undefined function %s" % function)


class Program:

    """
    Facilitates storage and lookup of the functions defined in the program.
    """

    def __init__(self, functions):
        self.functions = {}
        for i in functions:
            if i.name in self.functions:
                raise InterpreterError("Duplicate function name %s" % i.name)
            self.functions[i.name] = i

    def getFunction(self, name):
        if name not in self.functions:
            raise UndefinedFunction(name)
        return self.functions[name]


class Frame:

    """
    Represents a stack frame within the interpreter. Contains the environment
    of the frame as well the as current function, block and instruction.

    Note that whilst we don't actually create a real stack in the interpreter
    (instead we rely on Python's internal stack) the function of this class is
    broadly similar to that of a stack frame, so we borrow the terminology.
    """

    def __init__(self, program, func):
        self.program = program
        self.registers = {}
        self.variables = {}
        self.func = func
        self.block = func.getBlock(0)
        self.instruction = 0

    def getVariable(self, var):
        if var in self.variables:
            return self.variables[var]
        else:
            raise UndefinedVariable(var)

    def getRegister(self, reg):
        if reg in self.registers:
            return self.registers[reg]
        else:
            raise UndefinedRegister(reg)

    def fetchExecute(self):
        """
        Performs a fetch -> execute cycle until the given function returns,
        possibly recursing into other stack frames.
        """
        while True:
            # Fetch the next instruction and increment the program counter.
            inst = self.block.getInstruction(self.instruction)
            self.instruction += 1

            # Execute the instruction.
            if inst.op == "lc":
                # Load num arg[1] into register arg[0]
                self.registers[inst.args[0]] = inst.args[1]

            elif inst.op == "ld":
                # Load variable arg[1] into register arg[0]
                self.registers[inst.args[0]] = self.getVariable(inst.args[1])

            elif inst.op == "st":
                # Load register arg[1] into variable arg[0]
                self.variables[inst.args[0]] = self.getRegister(inst.args[1])

            elif inst.op == "add":
                # Store into register arg[0] the sum of registers arg[1] and
                # arg[2]
                self.registers[inst.args[0]] = (self.getRegister(inst.args[1])
                                                + self.getRegister(inst.args[2]))

            elif inst.op == "sub":
                # Store into register arg[0] the difference of registers arg[1]
                # and arg[2]
                self.registers[inst.args[0]] = (self.getRegister(inst.args[1])
                                                - self.getRegister(inst.args[2]))

            elif inst.op == "mul":
                # Store into register arg[0] the product of registers arg[1] and
                # arg[2]
                self.registers[inst.args[0]] = (self.getRegister(inst.args[1])
                                                * self.getRegister(inst.args[2]))

            elif inst.op == "div":
                # Store into register arg[0] the integer division of registers
                # arg[1] and arg[2]
                self.registers[inst.args[0]] = (self.getRegister(inst.args[1])
                                                // self.getRegister(inst.args[2]))

            elif inst.op == "lt":
                # Store into register arg[0] the result of registers arg[1] <
                # arg[2]
                self.registers[inst.args[0]] = int(self.getRegister(inst.args[1])
                                                   < self.getRegister(inst.args[2]))

            elif inst.op == "gt":
                # Store into register arg[0] the result of registers arg[1] >
                # arg[2]
                self.registers[inst.args[0]] = int(self.getRegister(inst.args[1])
                                                   > self.getRegister(inst.args[2]))

            elif inst.op == "eq" or inst.op == "cmp":
                # Store into register arg[0] the result of registers arg[1] ==
                # arg[2]
                self.registers[inst.args[0]] = int(self.getRegister(inst.args[1])
                                                   == self.getRegister(inst.args[2]))

            elif inst.op == "br":
                # Jump to block arg[1] if register arg[0] is non-zero. Otherwise
                # jump to block arg[2].
                self.instruction = 0
                if self.getRegister(inst.args[0]) != 0:
                    self.block = self.func.getBlock(inst.args[1])
                else:
                    self.block = self.func.getBlock(inst.args[2])

            elif inst.op == "ret":
                # Exit this function, returning the value in register arg[0]
                return self.getRegister(inst.args[0])

            elif inst.op == "call":
                # Call function arg[1] with the args arg[2:]. Store the return
                # value in register arg[0].

                # First we create a new stack frame for the function call.
                frame = Frame(
                    self.program,
                    self.program.getFunction(
                        inst.args[1]))

                # Next, check the correct number of arguments was provided.
                args = inst.args[2]
                if len(args) != len(frame.func.args):
                    raise InterpreterError("Not enough arguments to call %s. Expected %s, got %s"
                                           % (frame.func.name, len(frame.func.args), len(args)))

                # Populate the environment of the new frame with said
                # arguments.
                for i, register in enumerate(args):
                    frame.variables[
                        frame.func.args[i]] = self.getRegister(register)

                # Finally, run the frame's fetch execute cycle until completion
                # and store the result in the indicated register.
                self.registers[inst.args[0]] = frame.fetchExecute()

            else:
                # NOTE: The parser should guarantee that we don't see any unknown
                # operators, we if we get here then something has gone very badly
                # wrong.
                raise InterpreterError("Unknown operator %s" % inst.op)


class Function:

    """
    Represents a callable function in the input program.
    """

    def __init__(self, name, args, blocks):
        self.name = name
        self.args = args
        self.blocks = {}
        for b in blocks:
            if b.num in self.blocks:
                raise InterpreterError("Duplicate block number in function %s: %s"
                                       % (name, b.num))
            self.blocks[b.num] = b

    def getBlock(self, blockNum):
        if blockNum in self.blocks:
            return self.blocks[blockNum]
        raise InterpreterError("Cannot find block %s in func %s"
                               % (blockNum, self.name))


class Block:

    """
    Represents a block of instructions in the input program.
    """

    def __init__(self, num, instructions):
        self.num = num
        self.instructions = instructions

    def getInstruction(self, num):
        if num < len(self.instructions):
            return self.instructions[num]
        raise InterpreterError("No more instructions in block %s" % (self.num))


class Instruction:

    """
    Represents an instruction in the input program.
    """

    def __init__(self, op, args):
        self.op = op
        self.args = args
