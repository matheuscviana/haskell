module Biblioteca.Util where

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Util.hs

data Data = Dt Int Int Int deriving (Eq,Read,Show)

dataStr :: Data -> String
dataStr (Dt d m a) = dd ++ "/" ++ mm ++ "/" ++ aa
  where
    dd = if d > 9 then show d else "0" ++ show d
    mm = if m > 9 then show m else "0" ++ show m
    aa = show a

formatar :: String -> String -> String
formatar k v = chave ++ valor
  where
    chave = k ++ (replicate (29 - (length k)) '.') ++ ":"
    valor = (replicate (30 - (length v)) ' ') ++ v
