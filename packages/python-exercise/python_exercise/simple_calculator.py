def simple_calculator(s):
    num = 0
    stack = []
    sign = '+'
    
    for i, char in enumerate(s):
        if char.isdigit():
            num = num * 10 + int(char)
        
        if (not char.isdigit() and char != ' ') or i == len(s) - 1:
            if sign == '+':
                stack.append(num)
            elif sign == '-':
                stack.append(-num)
            elif sign == '*':
                stack.append(stack.pop() * num)
            elif sign == '/':
                stack.append(int(stack.pop() / num))
            
            sign = char
            num = 0
    
    return sum(stack) 