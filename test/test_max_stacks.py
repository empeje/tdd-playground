from src.max_stacks import MaxStack


def test_max_stacks():
    s = MaxStack()
    s.push(1)
    s.push(2)
    s.push(3)
    s.push(2)
    assert s.max() == 3
    s.pop()
    assert s.max() == 3
    s.pop()
    assert s.max() == 2