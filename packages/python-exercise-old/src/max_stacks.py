class MaxStack(object):
    def __init__(self):
        self.stack = []
        self.maxes = []

    def push(self, val):
        self.stack.append(val)
        self.maxes.append(val) if val > self.max() else self.maxes.append(self.max())

    def pop(self):
        self.stack.pop()
        if self.maxes:
            self.maxes.pop()

    def max(self):
        if self.maxes:
            return self.maxes[-1]
        else:
            return 0
