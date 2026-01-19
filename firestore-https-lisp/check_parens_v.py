
def check_parens_verbose(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    count = 0
    for i, line in enumerate(lines):
        for char in line:
            if char == '(':
                count += 1
            elif char == ')':
                count -= 1
        print(f"{i+1:3}: {count:2} | {line.strip()[:60]}")
    
    if count != 0:
        print(f"Total mismatch: {count}")

check_parens_verbose('src/main.lisp')
