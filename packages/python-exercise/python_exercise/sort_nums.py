def sort_nums(nums):
    if not nums:
        return []
    
    # Count occurrences of each number (0, 1, or 2)
    count = [0] * 3
    for num in nums:
        count[num] += 1
    
    # Reconstruct the array
    result = []
    for i in range(3):
        result.extend([i] * count[i])
    
    return result 