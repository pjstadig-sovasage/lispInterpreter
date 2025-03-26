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
            print(f"filepath: {filename}")
            controller.runFile(filename)
    else:
        print("Starting in REPL mode")
        controller.runREPL()
