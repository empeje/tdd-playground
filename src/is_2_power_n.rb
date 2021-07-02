class TwoPowerN
  def self.is_true(input)
    # given an integer: e.g. 8
    # return true if the number is one of the 2^n
    # for example: 8 -> true, karena 2^3
    # 6 -> false, karena ga ada factor yang 2^n
    # 7 -> false
    # 1024 -> true, 2^10
    # Algorithm

    # Hint: berpikir di basis-2
    # 1 true, 2^0: 1 -> 0
    # 2 true, 2^1,  10 -> 0
    # 4 true, 2^2,  100 -> 0
    # 8 true, 2^3,  1000 -> 0
    # 16 true, 2^3, 10000

    # n dicek 2 pangkat berapa?
    # disiapin dulu n
    # time: O(n)
    if input % 2 == 0
      true
    else
      false
    end
  end
end