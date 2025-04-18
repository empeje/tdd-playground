def permutations(nums):
    """
    Generate all possible permutations of the given list of numbers.
    
    This function uses a backtracking algorithm to generate all permutations:
    1. We maintain a 'start' index that divides the array into two parts:
       - Elements before 'start' are fixed in the current permutation
       - Elements from 'start' onward are candidates for the next position
    2. For each recursive call, we try all possible elements at the current position
       by swapping the element at 'start' with each element from 'start' to the end
    3. After each swap, we recursively generate permutations for the next position
    4. After the recursive call, we swap back (backtrack) to restore the original order
    5. When 'start' reaches the end of the array, we've completed a permutation and add it to results
    
    Args:
        nums: A list of elements to permute
        
    Returns:
        A list containing all possible permutations of the input list
    """
    def backtrack(start):
        if start == len(nums):
            # We've reached the end of the array, so we've completed a permutation
            # Make a copy of the current state and add it to our results
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