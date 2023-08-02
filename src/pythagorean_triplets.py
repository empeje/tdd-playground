def find_pythagorean_triplets(nums):
    for a in nums:
        for b in nums:
            for c in nums:
                if a*a + b*b == c*c:
                    return True
    return False


def find_pythagorean_triplets_2(nums):
    squares = set([n*n for n in nums])

    for a in nums:
        for b in nums:
            if a*a + b*b in squares:
                return True
    return False
