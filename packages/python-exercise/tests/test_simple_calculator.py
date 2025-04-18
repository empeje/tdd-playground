from python_exercise.simple_calculator import simple_calculator

def test_simple_calculator():
    assert simple_calculator("3+2*2") == 7
    assert simple_calculator(" 3/2 ") == 1
    assert simple_calculator(" 3+5 / 2 ") == 5 