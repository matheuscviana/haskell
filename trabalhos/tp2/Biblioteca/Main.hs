module Main (main) where

--Abra o ghci na pasta onde está a pasta Biblioteca.
--Não dentro da pasta Biblioteca.
--Então execute :l Biblioteca/Main.hs

import System.IO
import Data.Maybe
import Biblioteca.Util
import Biblioteca.Dados
import Biblioteca.Alunos
import Biblioteca.Livros
import Biblioteca.Emprestimos

main = do
  putStr "\n\n**MENU PRINCIPAL**"
  putStr "\n0 SAIR\n1 ALUNOS\n2 LIVROS\n3 EMPRÉSTIMOS"
  putStr "\nDigite sua opção: "
  op <- getLine
  case op of
    "0" -> return ()
    "1" -> do
      opalunos
      main
    "2" -> do
      oplivros
      main
    "3" -> do
      opemprestimos
      main
    _   -> main

opalunos :: IO()
opalunos = do
  op <- showmenu (Aln 0 "" "") --vide em Dados.hs o porquê dessa entrada
  case op of
    "0" -> return ()
    "1" -> do
      putStr "\nDigite o código de um aluno: "
      ca <- getLine
      al <- buscar (read ca) :: IO (Maybe Aluno)
      if al == Nothing
      then putStrLn "\nNenhum aluno cadastrado com o código informado."
      else imprimir (fromJust al)
      opalunos
    "2" -> do
      a <- cadastrar :: IO Aluno
      putStrLn "\nAluno cadastrado:"
      imprimir a
      opalunos
    "3" -> do
      putStr "\nDigite o código de um aluno: "
      ca <- getLine
      set <- obter :: IO (Set Aluno)
      apagar (read ca) set
      opalunos
    _   -> opalunos

oplivros :: IO()
oplivros = do
  op <- showmenu (Lvr 0 "" 0) --vide em Dados.hs o porquê dessa entrada
  case op of
    "0" -> return ()
    "1" -> do
      putStr "\nDigite o registro de um livro: "
      rg <- getLine
      lv <- buscar (read rg) :: IO (Maybe Livro)
      if lv == Nothing
      then putStrLn "\nNenhum livro cadastrado com o registro informado."
      else imprimir (fromJust lv)
      oplivros
    "2" -> do
      l <- cadastrar :: IO Livro
      putStrLn "\nLivro cadastrado:"
      imprimir l
      oplivros
    "3" -> do
      putStr "\nDigite o registro de um livro: "
      rg <- getLine
      set <- obter :: IO (Set Livro)
      apagar (read rg) set
      oplivros
    _   -> oplivros

opemprestimos :: IO()
opemprestimos = do
  op <- showmenu (Emp 0 (Aln 0 "" "") (Dt 0 0 0) (Dt 0 0 0) [])
  case op of
    "0" -> return ()
    "1" -> do
      putStr "\nDigite o número de um empréstimo: "
      nm <- getLine
      em <- buscar (read nm) :: IO (Maybe Emprestimo)
      if em == Nothing
      then putStrLn "\nNenhum livro cadastrado com o registro informado."
      else imprimir (fromJust em)
      opemprestimos
    "2" -> do
      e <- cadastrar :: IO Emprestimo
      putStrLn "\nEmpréstimo cadastrado:"
      imprimir e
      opemprestimos
    "3" -> do
      putStr "\nDigite o número de um empréstimo: "
      rg <- getLine
      set <- obter :: IO (Set Livro)
      apagar (read rg) set
      opemprestimos
    _   -> opemprestimos
