class Difference:
    def __init__(self, a):
        self.__elements = a
        self.maximumDifference = None
    # Add your code here
    def computeDifference(self):
        from itertools import combinations
        combinations_list = list(combinations(self.__elements,2))
        difference_list = []
        for subset in combinations_list:
            a = subset[0]
            b = subset[1]
            difference_list.append(abs(a-b))
        self.maximumDifference = max(difference_list)
# End of Difference class

_ = input()
a = [int(e) for e in input().split(' ')]

d = Difference(a)
d.computeDifference()

print(d.maximumDifference)
