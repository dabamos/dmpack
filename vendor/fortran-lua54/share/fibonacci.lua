-- fibonacci.lua
function fib(n)
   if n == 1 or n == 2 then
      return 1, 1
   end
   prev, prevPrev = fib(n - 1)
   return prev + prevPrev, prev
end
