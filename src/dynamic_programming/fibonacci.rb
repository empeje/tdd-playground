def fib(n)
  return 1 if n == 0 || n == 1
  fib(n - 1) + fib(n - 2)
end