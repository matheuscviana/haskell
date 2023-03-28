module Biblioteca.Livros (Livro(..)) where

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Livros.hs

import System.IO
import Biblioteca.Util
import Biblioteca.Dados

data Livro = Lvr {registro::Int, titulo::String, edicao::Int}
  deriving (Eq,Read,Show)

arquivo = "livros.txt"

instance Dado Livro where

  iden (Lvr r _ _) = r

  imprimir (Lvr r t e) = do
    putStrLn $ formatar "Registro" (show r)
    putStrLn $ formatar "Título" t
    putStrLn $ formatar "Edição" (show e)

  cadastrar = do
    putStrLn "\n\n**CADASTRO DE LIVRO**"
    putStr "Digite o registro: "
    r <- getLine
    putStr "Digite o título: "
    t <- getLine
    putStr "Digite a edição: "
    e <- getLine
    let livro = Lvr (read r) r (read e)
    arq <- openFile arquivo AppendMode
    hPutStrLn arq (show livro)
    hClose arq
    return livro

  obter = do
    arq <- openFile arquivo ReadMode
    set <- obteraux arq EmptySet
    hClose arq
    return set
      where
        obteraux :: Handle -> Set Livro -> IO (Set Livro)
        obteraux arq set = do
          fim <- hIsEOF arq
          if fim
          then return set
          else do
            str <- hGetLine arq
            obteraux arq (St (read str :: Livro) set)

  buscar i = do
    set <- obter :: IO (Set Livro)
    return (achar i set)

  apagar i set = do
    let s = remover i set
    arq <- openFile arquivo WriteMode
    atualizarArq arq s
    hClose arq
      where
        atualizarArq :: Handle -> Set Livro -> IO()
        atualizarArq _ EmptySet = return ()
        atualizarArq arq (St a s) = do
          hPutStrLn arq (show a)
          atualizarArq arq s

  showmenu _ = do
    putStr "\n\n**MENU LIVRO**"
    putStr "\n0 VOLTAR\n1 VIZUALIZAR\n2 CADASTRAR\n3 APAGAR"
    putStr "\nDigite sua opção: "
    op <- getLine
    return op
