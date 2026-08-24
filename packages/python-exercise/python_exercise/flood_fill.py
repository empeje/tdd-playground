"""Flood Fill algorithms (DFS and BFS implementations)."""

from collections import deque
from typing import List


def flood_fill_dfs(
    grid: List[List[int]], sr: int, sc: int, new_color: int
) -> List[List[int]]:
    """Perform flood fill on a 2D grid using Depth-First Search (DFS)."""
    if not grid or not grid[0]:
        return grid

    rows, cols = len(grid), len(grid[0])
    if not (0 <= sr < rows and 0 <= sc < cols):
        return grid

    target_color = grid[sr][sc]
    if target_color == new_color:
        return grid

    def dfs(r: int, c: int) -> None:
        if 0 <= r < rows and 0 <= c < cols and grid[r][c] == target_color:
            grid[r][c] = new_color
            dfs(r - 1, c)
            dfs(r + 1, c)
            dfs(r, c - 1)
            dfs(r, c + 1)

    dfs(sr, sc)
    return grid


def flood_fill_bfs(
    grid: List[List[int]], sr: int, sc: int, new_color: int
) -> List[List[int]]:
    """Perform flood fill on a 2D grid using Breadth-First Search (BFS)."""
    if not grid or not grid[0]:
        return grid

    rows, cols = len(grid), len(grid[0])
    if not (0 <= sr < rows and 0 <= sc < cols):
        return grid

    target_color = grid[sr][sc]
    if target_color == new_color:
        return grid

    queue = deque([(sr, sc)])
    grid[sr][sc] = new_color

    while queue:
        r, c = queue.popleft()
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == target_color:
                grid[nr][nc] = new_color
                queue.append((nr, nc))

    return grid
