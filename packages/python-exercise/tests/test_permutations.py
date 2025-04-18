from python_exercise.permutations import permutations

def test_permutations():
    assert sorted(permutations([1])) == [[1]]
    assert sorted(permutations([1, 2])) == [[1, 2], [2, 1]]
    assert sorted(permutations([1, 2, 3])) == [
        [1, 2, 3], [1, 3, 2], [2, 1, 3], 
        [2, 3, 1], [3, 1, 2], [3, 2, 1]
    ] 