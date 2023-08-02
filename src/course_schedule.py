class CourseSchedule:
    def _has_cycle(self, graph, course, seen):
        if course in seen:
            return True
        if course not in graph:
            return False

        seen.add(course)
        for neighbors in graph[course]:
            if self._has_cycle(graph, neighbors, seen):
                return True
        seen.remove(course)
        return False


    def can_finish(self, num_course, prerequisites):
        graph = {}
        for prereq in prerequisites:
            if prereq[0] in graph:
                graph[prereq[0]].append(prereq[1])
            else:
                graph[prereq[0]] = [prereq[1]]

        for course in range(num_course):
            if self._has_cycle(graph, course, set()):
                return False

        return True