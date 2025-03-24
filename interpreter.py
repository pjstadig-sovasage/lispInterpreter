from typing import List

class Function:
    def __init__(self, name: str, docstring: str, arguments: List[str]):
        self.name = name
        self.docstring = docstring
        self.arguments = arguments
        self.root = None
    
    def __repr__(self):
        return f"Function: {self.name} Arguments: {self.arguments} \n{self.docstring}"