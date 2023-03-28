
meudrop :: Int -> [Int] -> [Int]
meudrop _ [] = []
meudrop 0 ls = ls
meudrop x (a:b) = meudrop (x-1) b

--meutake :: Int -> [Int] -> [Int]
meutake _ [] = []
meutake 0 _ = []
meutake x (a:b) = a : meutake (x-1) b


inv [] = []
inv (a:b) = inv b ++ [a]

inverte [] = []
inverte ls = inverte' ls []

inverte' [] l = l
inverte' (a:b) l = inverte' b (a:l)

palindromo ls = ls == inv ls

(%%) :: Int -> Int -> Int
a %% b = a - (div a b) * b  


(%/) :: Int -> Int -> Int
a %/ b
  | a < b = a
  | otherwise = (a-b) %/ b  

con [] ls = ls
con ls [] = ls
con (a1:b2) l2 = a1 : con b2 l2  

a +++ b = a + b
infixl 6 +++

a -+- b = a - b
infixr 6 -+-
