from python_exercise.flood_fill import flood_fill_bfs, flood_fill_dfs


def test_flood_fill_dfs_basic():
    grid = [
        [1, 1, 1],
        [1, 1, 0],
        [1, 0, 1],
    ]
    expected = [
        [2, 2, 2],
        [2, 2, 0],
        [2, 0, 1],
    ]
    result = flood_fill_dfs(grid, 1, 1, 2)
    assert result == expected


def test_flood_fill_bfs_basic():
    grid = [
        [1, 1, 1],
        [1, 1, 0],
        [1, 0, 1],
    ]
    expected = [
        [2, 2, 2],
        [2, 2, 0],
        [2, 0, 1],
    ]
    result = flood_fill_bfs(grid, 1, 1, 2)
    assert result == expected


def test_flood_fill_same_color():
    grid = [
        [0, 0, 0],
        [0, 0, 0],
    ]
    result_dfs = flood_fill_dfs(grid, 0, 0, 0)
    assert result_dfs == [[0, 0, 0], [0, 0, 0]]

    result_bfs = flood_fill_bfs(grid, 0, 0, 0)
    assert result_bfs == [[0, 0, 0], [0, 0, 0]]


def test_flood_fill_out_of_bounds_or_empty():
    assert flood_fill_dfs([], 0, 0, 1) == []
    assert flood_fill_bfs([], 0, 0, 1) == []
    assert flood_fill_dfs([[1]], 5, 5, 2) == [[1]]
    assert flood_fill_bfs([[1]], 5, 5, 2) == [[1]]
