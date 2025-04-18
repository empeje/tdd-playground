from python_exercise.course_schedule import can_finish

def test_can_finish():
    assert can_finish(2, [[1,0]]) == True
    assert can_finish(2, [[1,0],[0,1]]) == False
    assert can_finish(3, [[1,0],[2,1]]) == True 