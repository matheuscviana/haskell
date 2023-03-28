import Data.Maybe (fromJust)

-- 1
type Nome = String
type Valor = Float
type Dinheiro = String
type Quantidade = Int
type Produto = (Nome, Valor)
type Item = (Produto, Quantidade)

produtos :: [Produto]
produtos = [
     ("AGUA MINERAL", 2.50),
     ("CERVEJA", 3.99),
     ("CHOCOLATE", 4.50),
     ("LEITE", 2.99)
  ]


--2
repete :: a -> Int -> [a]
repete _ 0 = []
repete c n = c : repete c (n-1)

repete2 :: a -> Int -> [a] --versão 2
repete2 c n = [c | x <- [1..n]]

--considerei que o primeiro elemento de uma lista fica na posição 1
--não 0 como no C e no Java
index :: Eq a => a -> [a] -> Maybe Int
index n lista = indexn n lista 1
  where
    indexn :: Eq a => a -> [a] -> Int -> Maybe Int
    indexn _ [] _    = Nothing
    indexn e (h:t) i = if e == h then Just i else indexn e t (i+1)

index2 :: Eq a => a -> [a] -> Maybe Int --versão 2
index2 e lista =  if null es then Nothing else Just (head es)
  where es = [ i | (i,x) <- zip [1..] lista, e == x]

elemento :: Int -> [a] -> Maybe a
elemento _ []    = Nothing
elemento 1 (p:r) = Just p
elemento n (p:r) = elemento (n-1) r

elemento2 :: Int -> [a] -> Maybe a --versão 2
elemento2 p lista = if null es then Nothing else Just (head es)
  where es = [ e | (i,e) <- zip [1..] lista, i == p]


--3
addProduto :: Produto -> [Produto] -> [Produto]
addProduto p lst = lst ++ [p]

remProduto :: Nome -> [Produto] -> [Produto]
remProduto _ [] = []
remProduto nm ((n,v):r)
  | nm == n   = r
  | otherwise = (n,v) : remProduto nm r

buscaProduto :: Nome -> [Produto] -> Maybe Produto
buscaProduto _ [] = Nothing
buscaProduto nm ((n,v):r)
  | nm == n   = Just (n,v)
  | otherwise = buscaProduto nm r


--4
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq s c n = s ++ repete c (n - (length s))

alinhaDir :: String -> Char -> Int -> String
alinhaDir s c n = repete c (n - (length s)) ++ s


--5
dinheiro :: Valor -> Dinheiro
dinheiro v = v $$ 2

($$) :: Valor -> Int -> Dinheiro
valor $$ n
  | f < n      = '$' : take (i+f) v ++ repete '0' (n-f)
  | otherwise  = '$' : take (i+n) v
  where v = show valor
        i = fromJust (index '.' v)
        f = (length v) - i
infix 5 $$


--6
formataItem :: Item -> String
formataItem ((n,v),q) =
  alinhaEsq n '.' 45 ++
  alinhaDir (" " ++ dinheiro v ++ " x " ++ show q ++ " = ") '.' 25 ++
  alinhaDir (dinheiro (v * fromIntegral q)) ' ' 10


--7
total :: [Item] -> Dinheiro
total li = dinheiro (calctotal li)
  where
    calctotal :: [Item] -> Valor
    calctotal []        = 0
    calctotal (((n,v),q):r) = (v * fromIntegral q) + (calctotal r)


--8
notafiscal :: [Item] -> String
notafiscal li = "\n" ++
  repete '*' linha ++ "\n" ++
  alinhaDir "NOTA FISCAL" ' ' ((div linha 2) + (div (length "NOTA FISCAL") 2)) ++ "\n" ++
  repete '*' linha ++ "\n" ++
  itensFormatados li ++ "\n" ++
  repete '*' linha ++ "\n" ++
  alinhaDir ("TOTAL: " ++ total li) ' ' linha ++ "\n" ++
  repete '*' linha ++ "\n"
 where
   linha :: Int
   linha = 80
   itensFormatados :: [Item] -> String
   itensFormatados [] = "\n"
   itensFormatados (i:r) = "\n" ++ formataItem i ++ itensFormatados r


--9
proditem :: [Produto] -> [Item]
proditem ps = proditemaux ps 1
  where
    proditemaux :: [Produto] -> Int -> [Item]
    proditemaux [] _    = []
    proditemaux (p:r) i = (p,i) : proditemaux r (i+1)

proditemq :: [Produto] -> [Quantidade] -> [Item]
proditemq _ []          = []
proditemq [] _          = []
proditemq (p:rp) (q:rq) = (p,q) : proditemq rp rq

proditemq2 :: [Produto] -> [Quantidade] -> [Item] --versão 2
proditemq2 produtos qtdes = zip produtos qtdes


--10
itensn :: [(Nome,Quantidade)] -> [Produto] -> [Item]
itensn [] _         = []
itensn _ []         = []
itensn ((n,q):r) ps = (fromJust (buscaProduto n ps), q) : itensn r ps

itensi :: [(Int,Quantidade)] -> [Produto] -> [Item]
itensi [] _         = []
itensi _ []         = []
itensi ((i,q):r) ps = (fromJust (elemento i ps), q) : itensi r ps


--11
venda :: [Item] -> IO()
venda itens = putStrLn (notafiscal itens)
