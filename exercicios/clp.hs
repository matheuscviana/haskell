soma :: Int -> Int -> Int
soma a b = a + b


fatorial :: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)


fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fat :: Int -> Int
fat n = if n == 0 
  then 1 
  else n * fat (n-1)

fibonacci :: Int -> Int
fibonacci n 
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = fibonacci (n-1) + fibonacci (n-2)

tamanho :: [Int] -> Int  
tamanho [] = 0
tamanho (h:t) = 1 + tamanho t

--remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove e [x] = if e == x then [] else [x]
remove e (h:t) = if e == h then t else h : remove e t

somalst [] = 0
somalst (h:t) = h + somalst t

removeTodos x lst = [ y  | y <- lst, y /= x]

selectionsort lst = m : selectionsort (remove m lst)
  where m = minimum lst






