from src.permutations import PermutationsSolution


def test_answer_approach_1():
    assert PermutationsSolution().permute([1, 2, 3]) == [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 2, 1],
                                                         [3, 1, 2]]


def test_answer_approach_2():
    assert PermutationsSolution().permute2([1, 2, 3]) == [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2],
                                                         [3, 2, 1]]
