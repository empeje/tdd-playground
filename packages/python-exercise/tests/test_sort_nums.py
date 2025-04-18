from python_exercise.sort_nums import sort_nums

def test_sort_nums():
    assert sort_nums([2,0,2,1,1,0]) == [0,0,1,1,2,2]
    assert sort_nums([2,0,1]) == [0,1,2]
    assert sort_nums([0]) == [0]
    assert sort_nums([1]) == [1] 