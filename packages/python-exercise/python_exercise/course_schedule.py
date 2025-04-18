from collections import defaultdict

def can_finish(num_courses, prerequisites):
    # Create adjacency list representation of the graph
    graph = defaultdict(list)
    for course, prereq in prerequisites:
        graph[course].append(prereq)

    # Keep track of visited nodes
    visited = set()
    # Keep track of nodes in current DFS path
    path = set()

    def has_cycle(course):
        # If node is already in current path, we found a cycle
        if course in path:
            return True
        # If we've already visited this node and found no cycle, we can skip it
        if course in visited:
            return False

        path.add(course)
        # Check all prerequisites of current course
        for prereq in graph[course]:
            if has_cycle(prereq):
                return True
        path.remove(course)
        visited.add(course)
        return False

    # Check for cycles starting from each course
    for course in range(num_courses):
        if has_cycle(course):
            return False

    return True 