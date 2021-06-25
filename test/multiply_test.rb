require "minitest/autorun"
require_relative "../src/multiply"

class TestMeme < Minitest::Test
  def test_can_multiply
    assert_equal 6, Multiply.calc(2, 3)
  end
end