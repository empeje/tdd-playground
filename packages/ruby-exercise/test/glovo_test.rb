require "minitest/autorun"

def fist_non_repeated_char(s)
  table = {}

  s = s.split('') # O(n) n is the length of string

  s.each do |c| # O(n)
    table[c] = 0
  end

  s.each do |c| # O(n)
    table[c] += 1
  end

  table.each_key do |key| # O(c)  constant is the number of a characters in ASCII 256
    return key if table[key] == 1
  end

  nil
end

class GlovoTest < Minitest::Test

  # Write an algorithm that returns the first non-repeated character in a string.
  # For example:.
  # firstNonRepeatedCharacter("ABCA")
  #
  # {"A" => 2,
  #  "B" => 1,
  # "C" => 1}
  # returns B.
  #  firstNonRepeatedCharacter("BCABAC")
  # returns null.
  #  firstNonRepeatedCharacter("BAC")
  # returns B.
  #  firstNonRepeatedCharacter("GlovoOnGlovo")
  # returns O.
  #  firstNonRepeatedCharacter("What is the first non-repeated character?") returns W


  def test_first
    assert_equal 'B', fist_non_repeated_char('ABCA')
  end

  def test_second
    assert_equal nil, fist_non_repeated_char('BCABAC')
  end

  def test_third
    assert_equal 'B', fist_non_repeated_char('BAC')
  end

  def test_fourth
    assert_equal 'O', fist_non_repeated_char('GlovoOnGlovo')
  end
end