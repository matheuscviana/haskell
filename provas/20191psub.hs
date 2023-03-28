--1
corta :: String -> Int -> (String,String)
corta s n = (take n s, drop n s)

--2
type Ponto = (Float,Float)
somalp :: [Ponto] -> Ponto
somalp [] = (0,0)
somalp [(x,y)] = (x,y)
somalp ((x1,y1):(x2,y2):t) = somalp ((x1+x2,y1+y2):t) 

--3
iniciais :: String -> [String]
iniciais s = iniaux s 0 (length s)

iniaux :: String -> Int -> Int -> [String]
iniaux s n t 
  | n < t = take n s : iniaux s (n+1) t 
  | otherwise = [s]

--4
maximo :: Ord a => [a] -> a --versão 1
maximo [x] = x
maximo (h:t) = if h > maximo t then h else maximo t

maximo2 :: Ord a => [a] -> a --versão 2
maximo2 ls = maximum ls 

--5
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao l1 l2 = [ x | x <- l1, elem x l2 ]

--6
filtra :: (a -> Bool) -> [a] -> [a] --versão 1
filtra f ls = [ x | x <- ls, f x ]

filtra2 :: (a -> Bool) -> [a] -> [a] --versão 2
filtra2 _ [] = []
filtra2 f (x:c)
  | f x = x : filtra2 f c
  | otherwise = filtra2 f c
