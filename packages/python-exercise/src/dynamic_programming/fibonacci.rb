def fib(n)
  a = 1
  b = 1
  (1...n).each do |_|
    a, b = b, a+b
  end
  b
end