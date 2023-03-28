--1
calcpi :: Int -> Double
calcpi 1 = 4
calcpi n =
  (if even n then -4.0 else 4.0) / fromIntegral (n+(n-1)) + calcpi (n-1)

calcpi2 :: Int -> Double --versão 2
calcpi2 n = calcpi2aux n 1 1
calcpi2aux :: Int -> Int -> Double -> Double
calcpi2aux n i d = (if even i then -4 else 4) / d +
  (if i == n then 0 else calcpi2aux n (i+1) (d+2))

--2
type Ponto = (Float,Float)
menorX :: [Ponto] -> Ponto
menorX [p] = p
menorX (h:t) =  pcmx h (menorX t)
  where
      pcmx :: Ponto -> Ponto -> Ponto
      pcmx a b = if fst a < fst b then a else b

--3
ocorre :: Eq a => a -> [a] -> Int
ocorre _ [] = 0
ocorre e (h:t)
  | e == h = 1 + ocorre e t
  | otherwise = ocorre e t

--4
semrepetido :: Eq a => [a] -> [a]
semrepetido [] = []
semrepetido (h:t) = h : semrepetido [x | x <- t, x /= h]

--5
aplica :: [(a -> b)] -> [a] -> [b]
aplica [] _ = []
aplica _ [] = []
aplica (f:rf) (x:rx) = f x : aplica rf rx
