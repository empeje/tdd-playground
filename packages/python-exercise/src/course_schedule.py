class CourseSchedule:
    def _has_cycle(self, graph, course, seen, cache):
        if course in cache:
            return cache[course]
        if course in seen:
            return True
        if course not in graph:
            return False

        seen.add(course)
        ret = False
        for neighbors in graph[course]:
            if self._has_cycle(graph, neighbors, seen, cache):
                ret = True
                break
        seen.remove(course)
        cache[course] = ret
        return ret


    def can_finish(self, num_course, prerequisites):
        graph = {}
        for prereq in prerequisites:
            if prereq[0] in graph:
                graph[prereq[0]].append(prereq[1])
            else:
                graph[prereq[0]] = [prereq[1]]

        for course in range(num_course):
            if self._has_cycle(graph, course, set(), {}):
                return False

        return True