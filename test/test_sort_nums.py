from src.sort_nums import sort_nums, sort_nums_2


def test_sort_nums():
    assert sort_nums([3, 3, 2, 1, 3, 2, 1]) == [1, 1, 2, 2, 3, 3, 3]


def test_sort_nums_2():
    assert sort_nums_2([3, 3, 2, 1, 3, 2, 1]) == [1, 1, 2, 2, 3, 3, 3]
