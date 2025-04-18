require "minitest/autorun"
=begin
  Write an algorithm that returns the first non-repeated character in a string.
  For example:.
  firstNonRepeatedCharacter("ABCA")
    returns B.
  firstNonRepeatedCharacter("BCABAC")
    returns null.
  firstNonRepeatedCharacter("BAC")
    returns B.
  firstNonRepeatedCharacter("MekariOMekari")
    returns O.
  firstNonRepeatedCharacter("What is the first non-repeated character?")
    returns W
=end

=begin

BIG O Notation

1. convert array to set ("ABCA")
2. set ["A", "B", "C"]
3. set[0]
=end

def solution(string) # "ABCA",
  # TIME O(N) + O(N) = O(2N) ~~ O(N)
  # SPACE O(N)

  array = string.split("") #  w[A B C A], O(N)
  array_exist = [] # [B C]
  array.each do |s| # O(N), N= 1000_000
    if array_exist.include?(s)
      array_exist.delete(s)
    else
      array_exist.push(s)
    end
  end
  array_exist[0]
end

class Fadhlir2Test < Minitest::Test
  def test_one
    assert_equal "B", solution("ABCA")
  end

  def test_two
    assert_equal nil, solution("BCABAC")
  end

  def test_three
    assert_equal "O", solution("MekariOMekari")
  end

  def test_four
	    assert_equal "B", solution("ABCA")
  end

  def test_five
    assert_equal "W", solution("What is the first non-repeated character?")
  end
end