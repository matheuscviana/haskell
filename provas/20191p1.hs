--1
quadrado :: Double -> Double -> Bool
quadrado a b = a == b

--2
caractere :: Char -> Char
caractere c 
  | c >= 'A' && c <= 'Z' = 'A'
  | c >= 'a' && c <= 'z' = 'a'
  | c >= '0' && c <= '9' = '0'
  | otherwise = '-'

--3
(+*) :: String -> Int -> String
_ +* 0 = ""
s +* n = s ++ (s +* (n-1))
infix 5 +*  

--4
qtd _ [] = 0
qtd e (a:b) = (if e == a then 1 else 0) + qtd e b 

--5
piv1 :: Int -> Double
piv1 1 = 4
piv1 n = (if mod n 2 == 0 then -1 else 1) * 4 / fromIntegral (n*2-1) + piv1 (n-1)

piv2 :: Int -> Double
piv2 n = piv2' n 1 1 0
piv2' :: Int -> Double -> Double -> Double -> Double
piv2' 0 _ _ pi = pi 
piv2' n p s pi = piv2' (n-1) (p+2) (-s) (pi + s * 4 / p)
