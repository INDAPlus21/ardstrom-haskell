module F1 where
import Data.Char
import Data.List.Split
-- Vad ska de andra funktionernas typsignaturer vara?
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = 
    let fibr 0 x y = x+y
        fibr i x y = fibr (i-1) y (x+y)
    in  fibr (n-2) 0 1


--Rövarspråket constants
consonants = "bcdfghjklmnpqrstvwxz"
vowels = "aeiouy"

--Rövarspråket encoder till och med z
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


--shitty inverse isAlpha function for medellagd
inverseIsAlpha :: Char -> Bool 
inverseIsAlpha x = isAlpha x == False



--medellangd of words in string calculator
medellangd :: String -> Double
medellangd [] = 0;
medellangd s = 
    let words = filter  (/= "") (splitWhen (inverseIsAlpha) s)
        nrOfWords = length words
    in  (fromIntegral(countAllTheWordsPlease words)) /  (fromIntegral nrOfWords)

countAllTheWordsPlease :: [String] -> Int
countAllTheWordsPlease [] = 0
countAllTheWordsPlease (x:xs) 
    | otherwise = (length x) + countAllTheWordsPlease xs





--medellangd s = 1.0


skyffla s = s
    