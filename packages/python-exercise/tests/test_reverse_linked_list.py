from python_exercise.reverse_linked_list import reverse_linked_list, ListNode

def test_reverse_linked_list():
    # Create test linked list: 1->2->3->None
    head = ListNode(1)
    head.next = ListNode(2)
    head.next.next = ListNode(3)
    
    # Reverse it
    reversed_head = reverse_linked_list(head)
    
    # Check values: 3->2->1->None
    assert reversed_head.val == 3
    assert reversed_head.next.val == 2
    assert reversed_head.next.next.val == 1
    assert reversed_head.next.next.next == None 