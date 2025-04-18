from python_exercise.max_stacks import MaxStack

def test_max_stack():
    stack = MaxStack()
    assert stack.get_max() == None
    assert stack.pop() == None

    stack.push(1)
    assert stack.get_max() == 1

    stack.push(2)
    assert stack.get_max() == 2

    stack.push(1)
    assert stack.get_max() == 2

    assert stack.pop() == 1
    assert stack.get_max() == 2 