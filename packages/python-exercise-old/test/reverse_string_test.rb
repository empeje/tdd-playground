require "minitest/autorun"
require_relative "../src/multiply"


# binary search tree - valid
#   2
#  /\
# 1  5
#   /\
# 3   7

a = [{id: "1", value: "", left_id: "2", right_id: ""}
,{id: "2", value: "", left_id: "", right_id: ""} ]

class Node
  attr_accessor :value, :left, :right
  def initialize(value, left=nil, right=nil)
    @value = value
    @left = left
    @right = right
  end
end

# -id-value-left_id-right_id
# 1, 2, 2, 3,
# 2, 1, , ,
# 3, 5, 4, 5,
# 4, 3, , ,
# 5, 7, , ,

# menentukan apakah binary search tree itu valid atau enggak

# invalid
#   2
#  /\
# 3  1

# invalid
#   2
#  /\
# 1  5
#   /\
#  7  8

def is_valid_tree(tree)

end

def length_of_last_word(word) # word = "makan nasi ayam    "
  array = word.split(" ") # ["makan", "nasi", "ayam"]
  array.last.length # "ayam"
end


def reverse_string(input) # "nakam"
  chars = input.split('') # ["n", "a", "k", "a", "m"]

  # ["n", "a", "k", "a", "m"]
  # chars[0] = "n"
  # chars[1] = "a"

  # 5 - 1 = 4
  # 5 - 2 = 3
  # 5 - 3 = 2
  # 5 - 4 = 1
  # 5 - 5 = 0

  reverse = [] # ["m"]
  (1..chars.length).each do |i|
      reverse << chars[chars.length-i]
  end
  reverse.join("")
end

class ReverseStringTest < Minitest::Test
  def test_can_reverse
    assert_equal 'makan', reverse_string('nakam')
  end
end