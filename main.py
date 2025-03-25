import sys
from controller import Controller

# Start program 
if __name__ == "__main__":
    controller = Controller()
    
    if "--file" in sys.argv:
        print("Parsing file...")
        fileIndex = sys.argv.index('--file')
        if fileIndex + 1 < len(sys.argv):
            filename = sys.argv[fileIndex + 1]
            Controller.runFileParse(filename)
    elif "--repl" in sys.argv:
        print("Entering REPL mode")
        Controller.runREPL()
    elif "--test" in sys.argv:
        print("Running test suite")
    else:
        print("No mode entered, starting in REPL")
        Controller.runREPL()
