from src.pythagorean_triplets import find_pythagorean_triplets, find_pythagorean_triplets_2


def test_pythagorean_triplets():
    assert not find_pythagorean_triplets([3, 5, 13, 5, 13])
    assert find_pythagorean_triplets([3, 5, 12, 5, 13])


def test_pythagorean_triplets_2():
    assert not find_pythagorean_triplets_2([3, 5, 13, 5, 13])
    assert find_pythagorean_triplets_2([3, 5, 12, 5, 13])