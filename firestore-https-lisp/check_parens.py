
def check_parens(filename):
    with open(filename, 'r') as f:
        content = f.read()
    
    count = 0
    for i, char in enumerate(content):
        if char == '(':
            count += 1
        elif char == ')':
            count -= 1
            if count < 0:
                print(f"Excess closing parenthesis at position {i}")
                return
    
    if count > 0:
        print(f"Missing {count} closing parentheses")
    elif count < 0:
        print(f"Excess {-count} closing parentheses")
    else:
        print("Parentheses balanced")

check_parens('src/main.lisp')
