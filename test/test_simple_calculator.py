from src.simple_calculator import SimpleCalculator


def test_simple_calculator():
    assert SimpleCalculator().eval('-(3+(2-1))') == -4
