module F1 where

-- Vad ska de andra funktionernas typsignaturer vara?
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = 
    let fibr 0 x y = x+y
        fibr i x y = fibr (i-1) y (x+y)
    in  fibr (n-2) 0 1


rovarsprak s = s
karpsravor s = s
medellangd s = 1.0
skyffla s = s