class PermutationsSolution(object):
    def _permutHelper(self, nums, start=0):
        if start == len(nums):
            return [nums[:]]

        result = []
        for i in range(start, len(nums)):
            nums[start], nums[i] = nums[i], nums[start]
            result += self._permutHelper(nums, start + 1)
            nums[start], nums[i] = nums[i], nums[start]
        return result


    def permute(self, nums):
        return self._permutHelper(nums)

    def permute2(self, nums, values=[]):
        if not nums:
            return [values]
        result = []
        for i in range(len(nums)):
            result += self.permute2(nums[:i] + nums[i+1:], values + [nums[i]])
        return result
