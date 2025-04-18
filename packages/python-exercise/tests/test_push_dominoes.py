from python_exercise.push_dominoes import push_dominoes

def test_push_dominoes():
    assert push_dominoes("RR.L") == "RR.L"
    assert push_dominoes(".L.R...LR..L..") == "LL.RR.LLRRLL.." 