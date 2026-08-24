import pytest
from python_exercise.canonical_bfs import bfs, bfs_with_levels


@pytest.fixture
def sample_graph():
    return {
        "A": ["B", "C"],
        "B": ["A", "D", "E"],
        "C": ["A", "F"],
        "D": ["B"],
        "E": ["B", "F"],
        "F": ["C", "E"],
    }


def test_bfs_traversal(sample_graph):
    order = bfs(sample_graph, "A")
    assert order == ["A", "B", "C", "D", "E", "F"]


def test_bfs_levels(sample_graph):
    levels = bfs_with_levels(sample_graph, "A")
    assert levels == {
        "A": 0,
        "B": 1,
        "C": 1,
        "D": 2,
        "E": 2,
        "F": 2,
    }


def test_bfs_empty_graph():
    assert bfs({}, "A") == []
    assert bfs_with_levels({}, "A") == {}


def test_bfs_missing_start_node(sample_graph):
    assert bfs(sample_graph, "Z") == []
    assert bfs_with_levels(sample_graph, "Z") == {}


def test_bfs_disconnected_node():
    graph = {
        "A": ["B"],
        "B": ["A"],
        "C": [],
    }
    assert bfs(graph, "A") == ["A", "B"]
    assert bfs_with_levels(graph, "A") == {"A": 0, "B": 1}
