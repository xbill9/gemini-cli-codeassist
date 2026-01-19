
import sys

def check_parens_stack(filename):
    with open(filename, 'r') as f:
        content = f.read()
    
    stack = []
    line = 1
    col = 1
    for i, char in enumerate(content):
        if char == '(':
            stack.append((line, col))
        elif char == ')':
            if not stack:
                print(f"Excess closing paren at line {line}, col {col}")
                return
            stack.pop()
        
        if char == '\n':
            line += 1
            col = 1
        else:
            col += 1
    
    if stack:
        print(f"Missing {len(stack)} closing parentheses. Unclosed at:")
        for l, c in stack:
            print(f"  Line {l}, col {c}")
    else:
        print("Balanced")

check_parens_stack('src/main.lisp')
