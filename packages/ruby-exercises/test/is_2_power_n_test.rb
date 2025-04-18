require "minitest/autorun"
require_relative "../src/is_2_power_n"

class TwoPowerNTest < Minitest::Test
  def test_can_check_8
    two_power_n = TwoPowerN.is_true(8)
    assert_equal true, two_power_n
  end

  def test_can_check_6
    two_power_n = TwoPowerN.is_true(6)
    # 2^2 = 4
    # 2^3 = 8
    assert_equal false, two_power_n
  end

  def test_can_check_10
    two_power_n = TwoPowerN.is_true(6)
    # 2^2 = 4
    # 2^3 = 8
    assert_equal false, two_power_n
  end

  def test_can_check_1m
    two_power_n = TwoPowerN.is_true(1000_000)
    assert_equal false, two_power_n
  end
end