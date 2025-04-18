require "minitest/autorun"

=begin
Given an array of integers e, arrange the elements in it so that:

e[0] ≤ e[1] ≥ e[2] ≤ e[3] ≥ e[4] ...

The elements in e need not be unique (they may be repeated)

[5 2 1 7 9 8] ->  1 ≤ 7 ≥ 5 ≤ 9 ≥ 2 ≤ 8  or  2 ≤ 5 ≥ 1 ≤ 9 ≥ 7 ≤ 8 or ...

1 2 3 4 5 6 ->  1 ≤ 3 ≥ 2 ≤ 5 ≥ 4 ≤ 6  or ...

-2 3 3 -3   ->  3 ≤ 3 ≥ -3 ≤ -2  or  -2 ≤ 3 ≥ -3 ≤ 3 or ...
=end

def solution
  e = [5, 2, 1, 7, 9, 8]
  r = [] # [1, ]
  e = e.sort # [1, 2, 5, 7, 8, 9]

  # [1, 2, 5, 7, 8, 9]
  # [1, 5, 2, 7, 8, 9]
  # [1, 5, 2, 8, 7, 9]

  i = 0

  while i <= e.length
    # do somehthing
    i+=2
  end

  e.each_with_index do |a, i|
    if i==0
      r.push(a)
    end
    if r.last == e[i] # 1 == 2
      r.push(e[i+1])
    else
      r.push(e[i])
    end
  end
end

class FadhlirTest < Minitest::Test

  def test_one
    assert_equal true, true
  end
end