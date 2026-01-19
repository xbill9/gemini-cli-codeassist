import sys

def check_parens_lisp(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        content = f.read()
    
    stack = []
    in_string = False
    escaped = False
    in_comment = False
    
    line = 1
    col = 1
    
    for i, char in enumerate(content):
        if escaped:
            escaped = False
        elif char == '\\':
            escaped = True
        elif char == '"':
            if not in_comment:
                in_string = not in_string
        elif char == ';':
            if not in_string and not in_comment:
                in_comment = True
        elif char == '\n':
            in_comment = False
            line += 1
            col = 1
            continue
        elif not in_string and not in_comment:
            if char == '(':
                stack.append((line, col))
            elif char == ')':
                if not stack:
                    print(f"Excess closing paren at line {line}, col {col}")
                    # return
                else:
                    stack.pop()
        
        col += 1
    
    if stack:
        print(f"Missing {len(stack)} closing parentheses. Unclosed at:")
        for l, c in stack:
            print(f"  Line {l}, col {c}")
    elif in_string:
        print("Unclosed string!")
    else:
        print("Balanced")

check_parens_lisp('src/main.lisp')