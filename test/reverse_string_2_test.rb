
def reverse_string(input)
  # input: makan
  # output: nakam
  # jadikan array
  # dapet panjangnya
end

class ReverseStringTest < Minitest::Test

  def test_can_reverse
    assert_equal 'makan', reverse_string('nakam')
  end
end