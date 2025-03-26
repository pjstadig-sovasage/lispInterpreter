from enum import Enum

class AtomType(Enum):
    NUMBER = "Number"
    STRING = "String"
    BOOLEAN = "Boolean"
    CHARACTER = "Character"
    SYMBOL = "Symbol"
    LIST = "List"
    
class BuiltIn(Enum):
    QUOTE = "Comment"
    IF = "If statement"
    COND = "Conditional statement"
    PROGN = "Sequential execution"
    LET = "Local assignment"
    LETSEQ = "Sequential local Assignment"
    SETQ = "Global assignment"
    LAMBDA = "Anonymous function"
    DEFUN = "Define function"
    DEFMACRO = "Define MACRO"
    QUASIQUOTE = "Define quasiquote"
    UNQUOTE = "Evaluate inside quasiquote"
    
    ADD = "+"
    SUB = "-"
    MULT = "*"
    DIV = "/"
    LEQ = "<="

# Used to match symbols to their AtomType/SpecialForm
reservedSymbols = {
    # Atoms
    "T" : AtomType.BOOLEAN,
    "NIL" : AtomType.BOOLEAN,
    
    # Built-In Functions
    "+" : BuiltIn.ADD,
    "-" : BuiltIn.SUB,
    "*" : BuiltIn.MULT,
    "<=" : BuiltIn.LEQ,
    
    # Special Forms
    "quote" : BuiltIn.QUOTE,
    "if" : BuiltIn.IF,
    "cond" : BuiltIn.COND,
    "progn" : BuiltIn.PROGN,

    "let" : BuiltIn.LET,
    "let*" : BuiltIn.LETSEQ,
    "setq" : BuiltIn.SETQ,    
    
    # Define functions/cacros
    "lambda" : BuiltIn.LAMBDA,
    "defun" : BuiltIn.DEFUN,
    "defmacro" : BuiltIn.DEFMACRO,
}