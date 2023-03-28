data Data = Dt {dia::Int, mes::Int, ano::Int} --deriving (Show)

instance Show Data where
    show d = show (dia d) ++ "/" ++ show (mes d) ++ "/" ++ show (ano d)

dataStr :: Data -> String
dataStr d = show (dia d) ++ "/" ++ show (mes d) ++ "/" ++ show (ano d)
--dataStr (Dt d m a) = show d ++ "/" ++ show d ++ "/" ++ show d

data Set t = EmptySet | St t (Set t) deriving (Show)

add :: Set t -> t -> Set t
add EmptySet v = (St v EmptySet)
add s v = (St v s)


formata :: String -> String -> String
formata c v = c ++ (replicate (29 - length c) '.') ++ ":" ++ (replicate (50 - length v) ' ') ++ v

class Dado t where
    cadastrar :: Set t -> IO (Set t)
    showmenu :: t -> IO Int 

data Aluno = Aln Int String String deriving (Show)












































instance Dado Aluno where
    cadastrar s = do
        putStr "Digite o codigo do aluno: "
        c <- getLine    
        putStr "Digite o nome do aluno: "
        n <- getLine    
        putStr "Digite o email do aluno: "
        e <- getLine
        return (add s (Aln (read c) n e))

    showmenu a = do
        putStrLn "\n0 VOLTAR\n1 CADASTRAR ALUNO"
        putStr "Digite sua opcao: "
        op <- getLine
        return (read op)
      
alunos :: Set Aluno        
alunos = (St (Aln 1 "JOAO" "JOAO@GMAIL.COM") (St (Aln 2 "MARIA" "MARIA") EmptySet))

main = do
    op <- showmenu (Aln 0 "" "")
    case op of
        0 -> return ()
        1 -> do
            as <- cadastrar alunos 
            putStrLn ("Alunos: " ++ show as)
            main
        _ -> main 
