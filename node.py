from typing import List

class Node: 
    def __init__(self, val, type, children : List = None):
        self.val = val
        self.type = type
        if not children:
            self.children = []
        else:
            self.children = children
    
    def print_tree(self, level=0):
        # Print the current node with an indentation based on its level in the tree
        print('  ' * level + f'({self.type}) {self.val}')
        
        # Recursively print all the children
        for child in self.children:
            child.print_tree(level + 1)