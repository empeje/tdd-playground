require "minitest/autorun"

class Node
  attr_accessor :val, :next
  def initialize(val)
    @val = val
    @next = nil
  end
end

class Solution
  def self.add_two_numbers(l1, l2)
    self.add_two_numbers_helper(l1, l2, 0)
  end

  def self.add_two_numbers_iterative(l1, l2)
    a = l1
    b = l2
    carry = 0
    ret = current = nil

    while a or b
      val = a.val + b.val + carry
      carry = val / 10

      if current
        current.next = Node.new(val % 10)
        current = current.next
      else
        ret = current = Node.new(val % 10)
      end

      if a.next or b.next
        a.next = Node.new(0) unless a.next
        b.next = Node.new(0) unless b.next
      end

      a = a.next
      b = b.next
    end
    ret
  end

  private

  def self.add_two_numbers_helper(l1, l2, carry)
    val = l1.val + l2.val + carry
    carry = val / 10
    ret = Node.new(val % 10)

    if l1.next || l2.next
      l1.next = Node.new(0) unless l1.next
      l2.next = Node.new(0) unless l2.next
      ret.next = self.add_two_numbers_helper(l1.next, l2.next, carry)
    end
    ret
  end
end

class AddNumbersLinkedListTest < Minitest::Test
  def test_one
    # 342
    l1 = Node.new(2)
    l1.next = Node.new(4)
    l1.next.next = Node.new(3)

    # 465
    l2 = Node.new(5)
    l2.next = Node.new(6)
    l2.next.next = Node.new(4)

    # 342 + 465 = 807 // 708
    res = Solution.add_two_numbers(l1, l2)
    assert_equal 7, res.val
    assert_equal 0, res.next.val
    assert_equal 8, res.next.next.val
  end

  def test_two
    # 342
    l1 = Node.new(2)
    l1.next = Node.new(4)
    l1.next.next = Node.new(3)

    # 465
    l2 = Node.new(5)
    l2.next = Node.new(6)
    l2.next.next = Node.new(4)

    # 342 + 465 = 807 // 708
    res = Solution.add_two_numbers_iterative(l1, l2)
    assert_equal 7, res.val
    assert_equal 0, res.next.val
    assert_equal 8, res.next.next.val
  end
end

