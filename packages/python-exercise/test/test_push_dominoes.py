from src.push_dominoes import PushDominoes


def test_push_dominoes():
    assert PushDominoes().push_dominoes('..R...L..R.') == "..RR.LL..RR"
