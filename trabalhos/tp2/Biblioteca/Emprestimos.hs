module Biblioteca.Emprestimos (Emprestimo(..)) where

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Emprestimos.hs

import System.IO
import Data.Maybe
import Biblioteca.Util
import Biblioteca.Dados
import Biblioteca.Alunos
import Biblioteca.Livros

data Emprestimo =
  Emp {numero::Int, aluno::Aluno, dtEmp::Data, dtDev::Data, livros::[Livro]}
  deriving (Eq,Read,Show)

arquivo = "emprestimos.txt"

instance Dado Emprestimo where

  iden (Emp n _ _ _ _) = n

  imprimir (Emp n a de dd ls) = do
    putStrLn $ formatar "Número" (show n)
    putStrLn "Aluno:"
    imprimir a
    putStrLn $ formatar "Data de Empréstimo" (dataStr de)
    putStrLn $ formatar "Data de Devolução" (dataStr dd)
    impLivros 0 ls
      where
        impLivros :: Int -> [Livro] -> IO()
        impLivros _ [] = return ()
        impLivros i (l:ls) = do
          putStrLn $ "Livro " ++ show i ++ ":"
          imprimir l
          impLivros (i+1) ls

  cadastrar = do
    putStrLn "\n\n**CADASTRO DE LIVRO**"
    putStr "Digite o número: "
    nm <- getLine
    al <- getAluno
    putStr "Digite a data de empréstimo (Dt d m a): "
    de <- getLine
    putStr "Digite a data de devolução (Dt d m a): "
    dd <- getLine
    ls <- getLivros []
    let emp = Emp (read nm) al (read de) (read dd) ls
    arq <- openFile arquivo AppendMode
    hPutStrLn arq (show emp)
    hClose arq
    return emp
      where
        getAluno :: IO Aluno
        getAluno = do
          putStr "Digite o código do aluno: "
          ca <- getLine
          al <- buscar (read ca)
          if al == Nothing
            then do
              putStrLn "\nNenhum aluno cadastrado com o código informado."
              getAluno
            else return (fromJust al)
        getLivros :: [Livro] -> IO [Livro]
        getLivros ls = do
          putStr "Digite o registro de um livro ou fim: "
          rl <- getLine
          if rl == "fim"
          then return ls
          else do
            lv <- buscar (read rl)
            if lv == Nothing
            then do
              putStrLn "\nNenhum livro cadastrado com o código informado."
              getLivros ls
            else getLivros (fromJust lv : ls)

  obter = do
    arq <- openFile arquivo ReadMode
    set <- obteraux arq EmptySet
    hClose arq
    return set
      where
        obteraux :: Handle -> Set Emprestimo -> IO (Set Emprestimo)
        obteraux arq set = do
          fim <- hIsEOF arq
          if fim
          then return set
          else do
            str <- hGetLine arq
            obteraux arq (St (read str :: Emprestimo) set)

  buscar i = do
    set <- obter :: IO (Set Emprestimo)
    return (achar i set)


  apagar i set = do
    let s = remover i set
    arq <- openFile arquivo WriteMode
    atualizarArq arq s
    hClose arq
      where
        atualizarArq :: Handle -> Set Emprestimo -> IO()
        atualizarArq _ EmptySet = return ()
        atualizarArq arq (St a s) = do
          hPutStrLn arq (show a)
          atualizarArq arq s


  showmenu _ = do
    putStr "\n\n**MENU EMPRÉSTIMO**"
    putStr "\n0 VOLTAR\n1 VIZUALIZAR\n2 CADASTRAR\n3 APAGAR"
    putStr "\nDigite sua opção: "
    op <- getLine
    return op
