def fib(n, cache={})
  return 1 if n == 0 || n == 1
  return cache[n] if cache[n]

  cache[n] = fib(n - 1, cache) + fib(n - 2, cache)
end