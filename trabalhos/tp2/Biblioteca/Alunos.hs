module Biblioteca.Alunos (Aluno(..)) where

import System.IO
import Biblioteca.Util
import Biblioteca.Dados

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Aluno.hs

data Aluno = Aln {codigo::Int, nome::String, email::String}
  deriving (Eq,Read,Show)

arquivo = "alunos.txt"

instance Dado Aluno where

  iden (Aln c _ _) = c

  imprimir (Aln c n e) = do
    putStrLn $ formatar "Código" (show c)
    putStrLn $ formatar "Nome" n
    putStrLn $ formatar "Email" e

  cadastrar = do
    putStrLn "\n\n**CADASTRO DE ALUNO**"
    putStr "Digite o código: "
    c <- getLine
    putStr "Digite o nome: "
    n <- getLine
    putStr "Digite o email: "
    e <- getLine
    let aluno = Aln (read c) n e
    arq <- openFile arquivo AppendMode
    hPutStrLn arq (show aluno)
    hClose arq
    return aluno

  obter = do
    arq <- openFile arquivo ReadMode
    set <- obteraux arq EmptySet
    hClose arq
    return set
      where
        obteraux :: Handle -> Set Aluno -> IO (Set Aluno)
        obteraux arq set = do
          fim <- hIsEOF arq
          if fim
          then return set
          else do
            str <- hGetLine arq
            obteraux arq (St (read str :: Aluno) set)

  buscar i = do
    set <- obter :: IO (Set Aluno)
    return (achar i set)

  apagar i set = do
    let s = remover i set
    arq <- openFile arquivo WriteMode
    atualizarArq arq s
    hClose arq
      where
        atualizarArq :: Handle -> Set Aluno -> IO()
        atualizarArq _ EmptySet = return ()
        atualizarArq arq (St a s) = do
          hPutStrLn arq (show a)
          atualizarArq arq s

  showmenu _ = do
    putStr "\n\n**MENU ALUNO**"
    putStr "\n0 VOLTAR\n1 VIZUALIZAR\n2 CADASTRAR\n3 APAGAR"
    putStr "\nDigite sua opção: "
    op <- getLine
    return op
