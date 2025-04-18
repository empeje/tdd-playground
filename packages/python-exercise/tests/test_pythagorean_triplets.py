from python_exercise.pythagorean_triplets import find_pythagorean_triplets

def test_find_pythagorean_triplets():
    assert find_pythagorean_triplets([3, 1, 4, 6, 5]) == True
    assert find_pythagorean_triplets([10, 4, 6, 12, 5]) == False
    assert find_pythagorean_triplets([3, 4, 5]) == True
    assert find_pythagorean_triplets([1, 2, 3]) == False 