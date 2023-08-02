class UniqueNumber(object):
    def single_number(self, nums):
        occurences = {}

        for n in nums:
            occurences[n] = occurences.get(n, 0) + 1

        for key, value in occurences.items():
            if value == 1:
                return key

    def single_number_2(self, nums):
        unique = 0
        for n in nums:
            unique ^= n
        return unique
