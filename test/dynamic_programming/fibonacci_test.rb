require "minitest/autorun"
require "benchmark"
require_relative "../../src/dynamic_programming/fibonacci"

class TestMeme < Minitest::Test
  def test_fibonacci
    assert_equal 3, fib(3)
  end

  def test_fibonacci
    how_long = Benchmark.measure do
      assert_equal 1346269, fib(30)
    end
    assert_operator how_long.total, :<, 0.01
  end
end