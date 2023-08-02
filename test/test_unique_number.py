from src.unique_number import UniqueNumber


def test_unique():
    assert UniqueNumber().single_number([4, 3, 2, 4, 1, 3, 2]) == 1


def test_unique_2():
    assert UniqueNumber().single_number_2([4, 3, 2, 4, 1, 3, 2]) == 1
