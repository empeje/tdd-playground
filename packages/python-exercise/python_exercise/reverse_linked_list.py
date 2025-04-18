class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def reverse_linked_list(head):
    prev = None
    current = head
    
    while current is not None:
        next_temp = current.next
        current.next = prev
        prev = current
        current = next_temp
    
    return prev 