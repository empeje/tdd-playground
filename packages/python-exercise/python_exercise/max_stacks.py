class MaxStack:
    def __init__(self):
        self.stack = []
        self.max_stack = []

    def push(self, x):
        self.stack.append(x)
        if not self.max_stack or x >= self.max_stack[-1]:
            self.max_stack.append(x)

    def pop(self):
        if not self.stack:
            return None
        x = self.stack.pop()
        if x == self.max_stack[-1]:
            self.max_stack.pop()
        return x

    def get_max(self):
        if not self.max_stack:
            return None
        return self.max_stack[-1] 