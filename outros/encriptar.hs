import Data.Char (isLetter, ord, chr)
import Data.String(lines, unlines)

encriptar :: String -> IO()
encriptar [] = putStr ""
encriptar s = putStr (unlines (encriptarLn n linhas))
  where
    n = read (head (lines s)) :: Int
    linhas = tail (lines s)

encriptarLn :: Int -> [String] -> [String]
encriptarLn _ [] = []
encriptarLn 0 _ = []
encriptarLn n (a:b) = (passadas a) : encriptarLn (n-1) b

desloca :: Char -> Int -> Char
desloca c i = chr ((ord c) + i)

passadas :: String -> String
passadas s = passada3 (passada2 (passada1 s))

passada1 :: String -> String
passada1 [] = []
passada1 (a:b) = (if isLetter a then desloca a 3 else a) : passada1 b

passada2 :: String -> String
passada2 s = reverse s

passada3 :: String -> String
passada3 s = metade1 ++ (passada3aux metade2)
  where
    metade  = div (length s) 2 
    metade1 = take metade s
    metade2 = drop metade s 
    passada3aux :: String -> String
    passada3aux [] = []
    passada3aux (a:b) = desloca a (-1) : passada3aux b
