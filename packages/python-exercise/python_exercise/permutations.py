def permutations(nums):
    def backtrack(start):
        if start == len(nums):
            result.append(nums[:])
            return
        
        for i in range(start, len(nums)):
            # Swap elements
            nums[start], nums[i] = nums[i], nums[start]
            # Recurse on the next position
            backtrack(start + 1)
            # Backtrack by swapping back
            nums[start], nums[i] = nums[i], nums[start]
    
    result = []
    backtrack(0)
    return result 