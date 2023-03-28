module Main where

import Agenda
import System.IO

menu :: String
menu = 
  "\n0 - sair\n1 - adicionar contato" ++
  "\n2 - obter telefones\n3 - imprimir agenda" ++
  "\nDigite sua opção: "

main = do mainaux AgVazia

mainaux :: Agenda -> IO()
mainaux ag = do
  putStr menu
  op <- getLine
  case op of
  	"0" -> return ()
  	"1" -> do
  		putStr "Digite um nome:"
  		n <- getLine
  		fs <- getFones []
  		mainaux (add n fs ag)
  	"2" -> do
  		putStr "Digite um nome:"
  		n <- getLine
  		putStrLn $ "Telefones de " ++ n ++ ": " ++ prtFones (busca n ag) 
  		mainaux ag
  	"3" -> do
  		putStrLn (show ag)
  		mainaux ag
  	_   -> mainaux ag 		

getFones :: [String] -> IO [String]
getFones fs = do
  putStr "Digite um telefone ou fim: "
  t <- getLine
  if t == "fim"
  then return fs
  else getFones (fs++[t])  		

prtFones :: [String] -> String
prtFones [] = ""
prtFones (h:t) = h ++ " " ++ prtFones t
