require "minitest/autorun"

def subarray?(arr)
  table = {} # O(m), m is the unique numbers in the array

  # space
  # time: O(n)
  sum = 0 # O(1) space complexity
  (0..(arr.length-1)).each do |i| # O(n), n is the length of the array
    sum += arr[i] # O(1)
    if table[sum] || sum == 0
      return true
    else
      table[sum] = true
    end
  end
  false
end

class Glovo2Test < Minitest::Test
  # Write an algorithm that given an array of integers, finds out
  # whether there is a subarray (of size at-least one) with 0 su.
  # Examples:
  # Input: [4, 2, -3, 1, 6]
  #
  # Experiment #3
  # [4, 2, -3, 1, 6]
  #
  # [4, 2, -3] [1, 6]
  #
  # zoom in [1, 6]
  # [1, 6] = 7 => false
  # [1] = 1  => false
  #
  # zoom [4, 2, -3]
  #
  # Experiment #1: [4, 2, -3, 1, 6]
  # [4 ] = 4
  # [4, 2] = 6
  # [4, 2, -3] = 3
  # [4, 2, -3, 1] = 4
  # [4, 2, -3, 1, 6] = 10
  #
  # { 4 => [[4], [4,2,-3,1]]}
  #
  #
  # Experiment #2
  # [4, 2, -3, 1, 6] = 10 # try to sum all
  #
  # [4, 2, -3, 1] = 4 # remove the right, and sum
  # [2, -3, 1, 6] = 6 # remove the left, and sum
  #
  # [2, -3, 1] = 0 # remove right and left, and sum
  #
  # Output: true
  # There is a subarray with zero sum from index 1 to 3.
  # Input: [4, 2, 0, 1, 6]
  #
  # [4] =4
  # [4, 2] = 6
  # [4, 2, 0] = 6
  # [4, 2, 0, 1] = 7
  # [4, 2, 0, 1, 6] = 13
  #
  # Experiment #2
  # [4, 2, 0, 1, 6] = 13
  #
  # [4, 2, 0, 1] = 7
  # [2, 0, 1, 6] = 9
  #
  # [2, 0, 1] = 3
  # [2, 0] = 2
  # [0, 1] = 1
  #
  # [0] = 0
  #
  # Output: true
  # There is a subarray with zero sum from index 2 to 2.
  # Input: [-3, 2, 3, 1, 6]
  # Output: falseThere is no subarray with zero sum.

  def test_one
    assert_equal true, subarray?([4, 2, -3, 1, 6])
  end

  def test_two
    assert_equal true, subarray?([4, 2, 0, 1, 6])
  end

  def test_three
    assert_equal false, subarray?([-3, 2, 3, 1, 6])
  end

  def test_four
    assert_equal true, subarray?([0,0,0])
  end

  def test_five
    assert_equal false, subarray?([-3,1,4,5,6,7,3])
  end

  def test_six
    assert_equal true, subarray?([-2, 5, -3])
  end

  def test_seven
    assert_equal true, subarray?([-5, 5])
  end
end