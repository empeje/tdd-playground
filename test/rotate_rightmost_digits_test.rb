require "minitest/autorun"
require_relative "../src/rotate_rightmost_digits"

class RotateRightmostDigitsTest < Minitest::Test
  [[1, 735291],
   [2, 735219],
   [3, 735912],
   [4, 732915],
   [5, 752913],
   [6, 352917]].each do |test_data|
    define_method "test_rotate_#{test_data.first}" do
      assert_equal test_data.last, rotate_rightmost_digits(735291, test_data.first)
    end
  end

end