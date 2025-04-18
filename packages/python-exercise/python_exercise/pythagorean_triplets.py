def find_pythagorean_triplets(arr):
    n = len(arr)
    squares = {}
    for i in range(n):
        squares[arr[i] * arr[i]] = i

    for i in range(n):
        for j in range(i + 1, n):
            square_sum = arr[i] * arr[i] + arr[j] * arr[j]
            if square_sum in squares:
                k = squares[square_sum]
                if k != i and k != j:
                    return True

    return False 