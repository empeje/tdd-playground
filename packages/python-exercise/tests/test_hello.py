"""Hello unit test module."""

from python_exercise.hello import hello


def test_hello():
    """Test the hello function."""
    assert hello() == "Hello python-exercise"
