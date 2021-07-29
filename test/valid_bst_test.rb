require "minitest/autorun"

class Node
  attr_accessor :value, :left, :right

  def initialize(value, left = nil, right = nil)
    @value = value
    @left = left
    @right = right
  end
end

class Solution
  def self.valid_bst?(n)
    self.valid_bst_helper(n, -Float::INFINITY, Float::INFINITY)
  end

  private

  def self.valid_bst_helper(n, low, high)
    return true unless n
    val = n.value
    if val > low && val < high && valid_bst_helper(n.left, low, n.value) && valid_bst_helper(n.right, n.value, high)
      true
    else
      false
    end
  end
end

class ValidBstTest < Minitest::Test
  def test_one
    #  5
    # / \
    #4   7
    node = Node.new(5)
    node.left = Node.new(4)
    node.right = Node.new(7)

    assert Solution.valid_bst?(node)
  end

  def test_two
    #  5
    # / \
    #4   7
    #  /
    # 2

    node = Node.new(5)
    node.left = Node.new(4)
    node.right = Node.new(7)
    node.right.left = Node.new(2)
    assert_equal false, Solution.valid_bst?(node)
  end

  def test_three
    #  5
    # / \
    #4   7
    #     \
    #      3
    node = Node.new(5)
    node.left = Node.new(4)
    node.right = Node.new(7)
    node.right.right = Node.new(3)
    assert_equal false, Solution.valid_bst?(node)
  end

  def test_four
    #  5
    # / \
    #4   7
    #     \
    #      9
    node = Node.new(5)
    node.left = Node.new(4)
    node.right = Node.new(7)
    node.right.right = Node.new(9)
    assert_equal true, Solution.valid_bst?(node)
  end
end
