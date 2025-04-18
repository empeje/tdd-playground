def sort_nums(nums):
    counts = {}
    for n in nums:
        counts[n] = counts.get(n, 0) + 1
    return [1] * counts.get(1, 0) + [2] * counts.get(2, 0) + [3] * counts.get(3, 0)

def sort_nums_2(nums):
    one_index = 0
    three_index = len(nums) - 1
    index = 0

    while index <= three_index:
        if nums[index] == 1:
            nums[index], nums[one_index] = nums[one_index], nums[index]
            one_index += 1
            index += 1
        if nums[index] == 2:
            index += 1
        if nums[index] == 3:
            nums[index], nums[three_index] = nums[three_index], nums[index]
            three_index -= 1
    return nums