from python_exercise.unique_number import find_unique_number

def test_find_unique_number():
    assert find_unique_number([2,2,1]) == 1
    assert find_unique_number([4,1,2,1,2]) == 4
    assert find_unique_number([1]) == 1 