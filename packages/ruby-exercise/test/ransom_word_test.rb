require "minitest/autorun"

class Solution
  def self.can_spell?(magazine, word)
    bags = {}
    magazine.each do |c|
      bags[c] = bags[c] ? bags[c]+=1 : 1
    end

    (0...word.length).each { |pos|
      return false unless bags[word[pos]]
      return false unless bags[word[pos]] > 0
      bags[word[pos]] -= 1
    }
    return true
  end
end

class RansomWordTest < Minitest::Test
  def test_one
    assert_equal true,  Solution.can_spell?(%w[a b c d e f], 'bcd')
  end

  def test_two
    assert_equal false, Solution.can_spell?(%w[a b c d e f], 'cat')
  end

end
