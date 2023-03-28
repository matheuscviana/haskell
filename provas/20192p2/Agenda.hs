module Agenda (Agenda(..), add, busca) where

add :: String -> [String] -> Agenda -> Agenda
busca :: String -> Agenda -> [String]

data Contato = Con {nome::String, fones::[String]}   
data Agenda = AgVazia | Ag Contato Agenda

instance Show Contato where
  show (Con n ts) = n ++ shw 1 ts
    where
      shw _ [] = []
      shw x (h:t) = " #" ++ show x ++ " " ++ h ++ shw (x+1) t  

instance Show Agenda where
  show AgVazia = ""
  show (Ag c ag) = show c ++ "\n" ++ show ag

add n ts AgVazia = Ag (Con n ts) AgVazia
add n ts (Ag c ag)
  | n < nome c = Ag (Con n ts) (Ag c ag)
  | otherwise  = Ag c (add n ts ag)

busca _ AgVazia = []
busca n (Ag c ag) = if n == nome c then fones c else busca n ag 
