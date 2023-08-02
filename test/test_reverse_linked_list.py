from src.reverse_linked_list import Node, Reverse


def test_reverse():
    node = Node(1, Node(2, Node(3, Node(4, Node(5)))))
    assert Reverse().reverse(node).val == 5
