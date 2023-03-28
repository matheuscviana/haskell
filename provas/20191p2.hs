--1
separa :: Eq a=> a -> [a] -> ([a],[a])
separa x ls = ([ e | e <- ls, e /= x],[ e | e <- ls, e == x])

--2
aplica :: ((a->b),(b->c),(c->d)) -> a -> d
aplica (f, g, h) x = (h.g.f) x

--3
type X = Float
type Y = Float
type Ponto = (X,Y)

menorX :: [Ponto] -> Ponto
menorX [p] = p
menorX (h:t) = if min xh (fst mt) == xh then h else mt
  where
    xh = fst h
    mt = menorX t

pontos :: [Ponto]    
pontos = [(1,2),(3,4),(0,6),(5,0)]

--4
data Contato = Con {nome::String,telefones::[String]} deriving (Show)

class Comparable t where
  comparar :: t -> t -> Int

instance Comparable Contato where
  comparar c1 c2 
    | nome c1 > nome c2 = 1
    | nome c1 < nome c2 = -1
    | otherwise = 0

ordenaContatos :: [Contato] -> [Contato]
ordenaContatos [] = []
ordenaContatos (p:r) = ordenaContatos me ++ [p] ++ ordenaContatos ma
  where
    me = [ c | c <- r, comparar c p == -1 ]
    ma = [ c | c <- r, comparar c p >= 0  ]

--5
-- (Bool, Int) é uma tupla binária em que o primeiro elemento é do tipo Bool e o segundo é do tipo Int
-- (Bool -> Int) é uma função unária que recebe um Bool e retorna um Int

--6
--(div 2) 5 = div 2 5 = 0
-- As funções são prefixas, de modo que o 5 fica como segundo operando. 
--(`div` 2) 5 = 5 `div` 2 = div 5 2 = 2
-- A crase é usada para tornar a função infixa, de modo que o 5 fica como primeiro operando.
