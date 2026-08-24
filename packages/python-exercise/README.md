# python-exercise

A collection of Test-Driven Development (TDD) algorithmic problems and domain service exercises implemented in Python.

## Problems

### Booking.com & General Algorithms
- [Name Chain](python_exercise/name_chain.py) - Find the longest chain of names where each name starts with the last letter of the previous name.
- [Closest Hotels](python_exercise/closest_hotels.py) - Find the K closest hotels from a given starting point.
- [Hotel List](python_exercise/hotel_list.py) - Filter and sort hotels based on multiple criteria.
- [Payment Logs](python_exercise/payment_logs.py) - Parse and analyze payment transaction logs.
- [Course Schedule](python_exercise/course_schedule.py) - Topological sort for course prerequisite dependencies.
- [Permutations](python_exercise/permutations.py) - Generate unique permutations.
- [Push Dominoes](python_exercise/push_dominoes.py) - Simulate domino physics.
- [Pythagorean Triplets](python_exercise/pythagorean_triplets.py) - Check for triplets satisfying $a^2 + b^2 = c^2$.
- [Reverse Linked List](python_exercise/reverse_linked_list.py) - Reverse singly-linked lists.
- [Simple Calculator](python_exercise/simple_calculator.py) - Basic arithmetic expression evaluation.
- [Sort Nums](python_exercise/sort_nums.py) - Custom number sorting algorithms.
- [Max Stacks](python_exercise/max_stacks.py) - Stack data structure tracking max element in $O(1)$.
- [Unique Number](python_exercise/unique_number.py) - Find unique element in array.

### FareHarbor Practice Problems
- [FareHarbor Daily Summary](python_exercise/fareharbor_daily.py) - Domain service calculating booking counts and revenue summaries.
- [Aggregate Revenue (Dict Processing)](python_exercise/dict_processing.py) - Aggregate revenue per product across nested regional sales data.
- [Canonical BFS](python_exercise/canonical_bfs.py) - Breadth-First Search traversal and level distance computation.
- [Flood Fill](python_exercise/flood_fill.py) - 2D grid flood fill implementation with DFS and BFS.
- [Shortest Path BFS](python_exercise/shortest_path_bfs.py) - Find the shortest distance between two nodes in an unweighted graph.
- [Sanitize JSON](python_exercise/sanitize_json.py) - Recursively sanitize sensitive keys from nested JSON structures.
- [Flatten Object](python_exercise/flatten_object.py) - Flatten nested dictionaries and lists into dot notation.

## Running Tests

```bash
# Using uv and pytest
uv run pytest packages/python-exercise/tests/

# Using Nx
pnpm dlx nx test python-exercise --no-cloud
```
