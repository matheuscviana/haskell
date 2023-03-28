main = let 
  x = 1
  y = 2
  z = 3
in putStrLn $ "X = " ++ show x ++ "\nY = " ++ show y ++ "\nZ = " ++ show z



fatorial :: Int -> Int
fatorial n = fatcauda n 1



fatcauda :: Int -> Int -> Int
fat n f | n == 1 = f 
        | otherwise = fatcauda (n-1) (f * n)



mdc :: Int -> Int -> Int
mdc a b 
  | mod a b == 0 = b
  | mod b a == 0 = a
  | a > b = mdc b (mod a b)
  | a < b = mdc a (mod b a)



mmc :: Integral a => a -> a -> a
mmc a b 
  | a == 0 = 0
  | b == 0 = 0
  | a == b = a
  | otherwise = div (a * b) (mdc a b)



maior :: [Int] -> Int
maior [a] = a
maior (a:b) 
  | a > (maior b) = a
  | otherwise     = maior b 


  
tamanho [] = 0
tamanho (_:c) = 1 + tamanho c



selectionsort [] = []
selectionsort lst = menor : selectionsort (retira menor lst)
					where menor = minimum lst



retiraTodos a lst = [x | x <- lst, x /= a]

retiraTodos2 a lst = filter (/=a) lst



retira :: Ord a => a -> [a] -> [a] 
retira _ [] = []
retira a (h:t) = if a == h then t else h : retira a t  



(!=) :: Int -> Int -> Bool
a != b = a /= b

(+*) :: Char -> Int -> String
c +* 0 = ""
c +* x = c : (c +* (x-1))

infix 4 !=
infixl 8 +*



quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:r) = quicksort [ y | y <- r, y < p ] ++ [p] ++ quicksort [ y | y <- r, y >= p ]



mergesort []  = []
mergesort [x] = [x]
mergesort lst = conquistar (mergesort m1) (mergesort m2)
				where 
					metade = div (length lst) 2
					m1 = take metade lst
					m2 = drop metade lst
          conquistar [] lst = lst
          conquistar lst [] = lst
          conquistar (p1:r1) (p2:r2)
            | p1 < p2   = p1 : conquistar r1 (p2:r2)
            | otherwise = p2 : conquistar (p1:r1) r2 
