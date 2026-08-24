from python_exercise.shortest_path_bfs import shortest_path_bfs


def test_shortest_path_bfs_standard():
    graph = {
        "A": ["B", "C"],
        "B": ["A", "D", "E"],
        "C": ["A", "F"],
        "D": ["B"],
        "E": ["B", "F"],
        "F": ["C", "E"],
    }
    assert shortest_path_bfs(graph, "A", "F") == 2
    assert shortest_path_bfs(graph, "A", "B") == 1
    assert shortest_path_bfs(graph, "A", "D") == 2


def test_shortest_path_same_start_end():
    graph = {"A": ["B"], "B": ["A"]}
    assert shortest_path_bfs(graph, "A", "A") == 0


def test_shortest_path_unreachable():
    graph = {
        "A": ["B"],
        "B": ["A"],
        "C": ["D"],
        "D": ["C"],
    }
    assert shortest_path_bfs(graph, "A", "C") == -1


def test_shortest_path_empty_or_missing_nodes():
    assert shortest_path_bfs({}, "A", "B") == -1
    assert shortest_path_bfs({"A": []}, "A", "B") == -1
    assert shortest_path_bfs({"A": []}, "Z", "A") == -1
