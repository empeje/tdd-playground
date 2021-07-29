require "minitest/autorun"

class Solution
  def self.two_sums(arr, target)
    bags = {}
    arr.each_with_index do |n, i|
      bags[n] = i
      return [bags[n], bags[target-n]] if bags[target-n]
    end
    []
  end
end

class TwoSumsTest < Minitest::Test
  def test_one
    assert_equal [1,2], Solution.two_sums([2,7,11,15], 18).sort
  end

  def test_two
    assert_equal [], Solution.two_sums([2,7,11,15], 100)
  end
end