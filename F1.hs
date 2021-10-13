module F1 where

-- Vad ska de andra funktionernas typsignaturer vara?
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = 
    let fibr 0 x y = x+y
        fibr i x y = fibr (i-1) y (x+y)
    in  fibr (n-2) 0 1


--Rövarspråket encoder till och med z
consonants = "bcdfghjklmnpqrstvwxz"
vowels = "aeiouy"

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs)
    | x `elem` consonants = [x] ++ "o" ++ [x] ++ next
    | otherwise = [x] ++ next
    where next = rovarsprak xs


--Rövarspråket decoder till och med z
karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs)
    | x `elem` consonants = [x] ++ karpsravor (drop 2 xs)
    | otherwise = [x] ++ next
    where next = karpsravor xs



medellangd s = 1.0
skyffla s = s
    