module Biblioteca.Dados(Dado(..), Set(..), achar, inserir, remover) where

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Dados.hs

--Descobri que é obrigatório que nas funções de uma classe
--ao menos um dos parâmetros deve ter a variável do tipo
--por isso as funções cadastrar e showmenu têm uma entrada/saída do tipo t
--não era necessário, mas o Haskell obrigou
class Dado t where
  iden :: t -> Int --obtem o valor do campo identificador do Dado
  imprimir :: t -> IO()
  cadastrar :: IO t
  obter :: IO (Set t)
  buscar :: Int -> IO (Maybe t)
  apagar :: Int -> Set t -> IO()
  showmenu :: t -> IO String

data Set t = EmptySet | St t (Set t) deriving (Show)

--troquei o nome para não conflitar com a função buscar da classe Dado
achar :: Dado a => Int -> Set a -> Maybe a
achar _ EmptySet = Nothing
achar i (St d s) = if i == iden d then Just d else achar i s

contem :: Dado a => a -> Set a -> Bool --fiz para ajudar o inserir
contem _ EmptySet = False
contem x (St d s) = if iden x == iden d then True else contem x s

inserir :: Dado a => a -> Set a -> Set a
inserir d EmptySet = St d EmptySet
inserir d s = if contem d s then s else St d s

remover :: Dado a => Int -> Set a -> Set a
remover _ EmptySet = EmptySet  --se i não existir, mantém o set como está
remover i (St d s) = if i == iden d then s else St d (remover i s)

vazio :: Set a -> Bool
vazio EmptySet = True
vazio _ = False
