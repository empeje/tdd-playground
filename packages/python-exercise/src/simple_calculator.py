class SimpleCalculator(object):
    def _eval_helper(self, expression, index):
        op = '+'
        result = 0
        while index < len(expression):
            char = expression[index]
            if char in ('+', '-'):
                op = char
            else:
                value = 0
                if char.isdigit():
                    value = int(char)
                elif char == '(':
                    (value, index) = self._eval_helper(expression, index + 1)
                if op == '+':
                    result += value
                if op == '-':
                    result -= value
            index += 1
        return (result, index)

    def eval(self, expression):
        return self._eval_helper(expression, 0)[0]