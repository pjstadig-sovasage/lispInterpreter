from typing import List

class Node: 
    def __init__(self, val, type, children : List = None):
        self.val = val
        self.type = type
        if not children:
            self.children = []
        else:
            self.children = children
    
    def __repr__(self):
        return self.astString()
    
    def build_tree(self, level=0):
        return '  ' * level + f'({self.type}) {self.val}'
       
    def astString(self, level=0) -> str:
        # Initialize the result string with the current node
        result = '  ' * level + f'({self.type}) {self.val}\n'
        
        # Recursively add the children to the result string
        for child in self.children:
            result += child.astString(level + 1)
        
        return result