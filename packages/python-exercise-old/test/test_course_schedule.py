from src.course_schedule import CourseSchedule


def test_course_schedule():
    assert not CourseSchedule().can_finish(2, [[1, 0], [0, 1]])
    assert CourseSchedule().can_finish(2, [[1, 0]])
