-- VERIFICACAO ---------------------------------------
converte2 :: [Char] -> Int
converte2 x = read x
 
converte :: [Char] -> [Int]
converte [] = []
converte (x:y) = converte2 [x]:converte y

maiorMultiplo :: Int -> Int
maiorMultiplo x = (x `div` 10)*10 + 10

codigoValido2 :: [Int] -> Int
codigoValido2 [] = 0
codigoValido2 (x:y)
   |length (x:y) `mod` 2 == 0 = 3*x + codigoValido2 y
   |length (x:y) == 1 = 0
   |otherwise = x + codigoValido2 y
 
codigoValido :: [Int] -> Bool
codigoValido x =
   if maiorMultiplo(codigoValido2 x) - codigoValido2 x == last x && length x==13 then
      True
   else
      False

mensagem :: Bool -> String
mensagem x =
   if x==True then
      "Codigo VALIDO\n"
   else
      "Codigo INVALIDO\n"
-- FIM VERIFICACAO ------------------------------------------------------------

-- Transformacao em String de 0s e 1s
codigoA :: Int -> String
codigoA x
   |x == 0 = "0001101"
   |x == 1 = "0011001"
   |x == 2 = "0010011"
   |x == 3 = "0111101"
   |x == 4 = "0100011"
   |x == 5 = "0110001"
   |x == 6 = "0101111"
   |x == 7 = "0111011"
   |x == 8 = "0110111"
   |x == 9 = "0001011"

codigoB :: Int -> String
codigoB x
   |x == 0 = "0100111"
   |x == 1 = "0110011"
   |x == 2 = "0011011"
   |x == 3 = "0100001"
   |x == 4 = "0011101"
   |x == 5 = "0111001"
   |x == 6 = "0000101"
   |x == 7 = "0010001"
   |x == 8 = "0001001"
   |x == 9 = "0010111"
   
codigoC :: Int -> String
codigoC x
   |x == 0 = "1110010"
   |x == 1 = "1100110"
   |x == 2 = "1101100"
   |x == 3 = "1000010"
   |x == 4 = "1011100"
   |x == 5 = "1001110"
   |x == 6 = "1010000"
   |x == 7 = "1000100"
   |x == 8 = "1001000"
   |x == 9 = "1110100"

dig13_0 :: [Int] -> [String]
dig13_0 [] = []
dig13_0 (x:y) =
   if length (x:y) > 6 then
      codigoA x:dig13_0 y 
   else
      codigoC x:dig13_0 y

dig13_1 :: [Int] -> [String]
dig13_1 [] = []
dig13_1 (x:y)
   |length (x:y) == 12 || length (x:y) == 11 || length (x:y) == 9 = codigoA x:dig13_1 y
   |length (x:y) == 10 || length (x:y) == 8 || length (x:y) == 7 = codigoB x:dig13_1 y
   |otherwise = codigoC x:dig13_1 y
   
dig13_2 :: [Int] -> [String]
dig13_2 [] = []
dig13_2 (x:y)
   |length (x:y) == 12 || length (x:y) == 11 || length (x:y) == 8 = codigoA x:dig13_2 y
   |length (x:y) == 10 || length (x:y) == 9 || length (x:y) == 7 = codigoB x:dig13_2 y
   |otherwise = codigoC x:dig13_2 y
   
dig13_3 :: [Int] -> [String]
dig13_3 [] = []
dig13_3 (x:y)
   |length (x:y) == 12 || length (x:y) == 11 || length (x:y) == 7 = codigoA x:dig13_3 y
   |length (x:y) == 10 || length (x:y) == 9 || length (x:y) == 8 = codigoB x:dig13_3 y
   |otherwise = codigoC x:dig13_3 y
   
dig13_4 :: [Int] -> [String]
dig13_4 [] = []
dig13_4 (x:y)
   |length (x:y) == 12 || length (x:y) == 10 || length (x:y) == 9 = codigoA x:dig13_4 y
   |length (x:y) == 11 || length (x:y) == 8 || length (x:y) == 7 = codigoB x:dig13_4 y
   |otherwise = codigoC x:dig13_4 y
   
dig13_5 :: [Int] -> [String]
dig13_5 [] = []
dig13_5 (x:y)
   |length (x:y) == 12 || length (x:y) == 9 || length (x:y) == 8 = codigoA x:dig13_5 y
   |length (x:y) == 11 || length (x:y) == 10 || length (x:y) == 7 = codigoB x:dig13_5 y
   |otherwise = codigoC x:dig13_5 y
   
dig13_6 :: [Int] -> [String]
dig13_6 [] = []
dig13_6 (x:y)
   |length (x:y) == 12 || length (x:y) == 8 || length (x:y) == 7 = codigoA x:dig13_6 y
   |length (x:y) == 11 || length (x:y) == 10 || length (x:y) == 9 = codigoB x:dig13_6 y
   |otherwise = codigoC x:dig13_6 y
   
dig13_7 :: [Int] -> [String]
dig13_7 [] = []
dig13_7 (x:y)
   |length (x:y) == 12 || length (x:y) == 10 || length (x:y) == 8 = codigoA x:dig13_7 y
   |length (x:y) == 11 || length (x:y) == 9 || length (x:y) == 7 = codigoB x:dig13_7 y
   |otherwise = codigoC x:dig13_7 y
   
dig13_8 :: [Int] -> [String]
dig13_8 [] = []
dig13_8 (x:y)
   |length (x:y) == 12 || length (x:y) == 10 || length (x:y) == 7 = codigoA x:dig13_8 y
   |length (x:y) == 11 || length (x:y) == 9 || length (x:y) == 8 = codigoB x:dig13_8 y
   |otherwise = codigoC x:dig13_8 y
   
dig13_9 :: [Int] -> [String]
dig13_9 [] = []
dig13_9 (x:y)
   |length (x:y) == 12 || length (x:y) == 9 || length (x:y) == 7 = codigoA x:dig13_9 y
   |length (x:y) == 11 || length (x:y) == 10 || length (x:y) == 8 = codigoB x:dig13_9 y
   |otherwise = codigoC x:dig13_9 y
   
transforma2 :: Int -> [Int] -> [String]
transforma2 n [] = []
transforma2 n x
   |n == 0 = dig13_0 x
   |n == 1 = dig13_1 x
   |n == 2 = dig13_2 x
   |n == 3 = dig13_3 x
   |n == 4 = dig13_4 x
   |n == 5 = dig13_5 x
   |n == 6 = dig13_6 x
   |n == 7 = dig13_7 x
   |n == 8 = dig13_8 x
   |n == 9 = dig13_9 x

converteParaString2 :: Int -> String
converteParaString2 x = show x
   
converteParaString :: [Int] -> String
converteParaString [] = ""
converteParaString (x:y) = (converteParaString2 x)++(converteParaString y)
   
transforma :: [Int] -> [String]
transforma (x:y) = (converteParaString (x:y)):"0000000":transforma2 x y
-- FIM da Transformacao em String de 0s e 1s -----------------------------------------

-- Formacao da imagem SVG
escreveCodigo2 :: Double -> Double -> String -> String
escreveCodigo2 pos h num = "<text x=\"" ++ (show pos) ++ "\" y=\"" ++ (show h) ++ "\" font-family=\"Arial\" font-size=\"4\" fill=\"black\">" ++ num ++ "</text>\n"

escreveCodigo :: Double -> Double -> String -> String
escreveCodigo pos h [] = ""
escreveCodigo pos h (x:y)
   |length (x:y) == 13 = (escreveCodigo2 (pos+1.0) h [x])++(escreveCodigo (pos+4.9) h y)
   |length (x:y) < 13 && length (x:y) > 6 = (escreveCodigo2 pos h [x])++(escreveCodigo (pos+2.3) h y)
   |length (x:y) == 6 = (escreveCodigo2 (pos+1.3) h [x])++(escreveCodigo (pos+3.6) h y)
   |length (x:y) < 6 = (escreveCodigo2 pos h [x])++(escreveCodigo (pos+2.3) h y)

desenhaBarras3 :: Double -> Double -> String -> String
desenhaBarras3 pos h cor = "<rect x=\"" ++ (show pos) ++ "\" y=\"0\" width=\"0.33\" height=\"" ++ (show h) ++ "\" style=\"stroke-width:0;fill:" ++ cor ++ "\"/>\n"

desenhaBarras2 :: Double -> Double -> String -> String
desenhaBarras2 pos h [] = ""
desenhaBarras2 pos h (x:y)
   |x == '1' = (desenhaBarras3 pos h "black") ++ (desenhaBarras2 (pos+0.330) h y)
   |x == '0' = (desenhaBarras3 pos h "white") ++ (desenhaBarras2 (pos+0.330) h y)

desenhaBarras :: Double -> [String] -> String
desenhaBarras pos [] = (desenhaBarras2 pos 25.40 "101")++(desenhaBarras2 (pos+0.99) 22.85 "0000000")
desenhaBarras pos (x:y)
   |length (x:y) == 14 = (escreveCodigo pos 26.0 x)++(desenhaBarras pos y)
   |length (x:y) == 13 = (desenhaBarras2 pos 22.85 x)++(desenhaBarras2 (pos+3.630) 25.40 "101")++(desenhaBarras (pos+4.620) y)
   |length (x:y) < 13 && length (x:y) > 6 = (desenhaBarras2 pos 22.85 x)++(desenhaBarras (pos+2.31) y)
   |length (x:y) == 6 = (desenhaBarras2 pos 25.40 "01010")++(desenhaBarras2 (pos+1.65) 22.85 x)++(desenhaBarras (pos+3.96) y)
   |length (x:y) < 6 && length (x:y) > 0 = (desenhaBarras2 pos 22.85 x)++(desenhaBarras (pos+2.31) y)
   |otherwise = (desenhaBarras2 pos 25.40 "101")++(desenhaBarras2 (pos+0.99) 22.85 "0000000")
   
arquivoSVG :: String -> String
arquivoSVG s = "<svg width=\"37.29mm\" height=\"25.93mm\" viewBox=\"0 0 37.29 25.93\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++ s ++ "</svg>"
-- FIM da formacao da imagem SVG ------------------------------------------------------

main = do
   putStr "Digite o codigo: "
   cod <- getLine
   if codigoValido (converte cod) == True then
      writeFile "cod_barra.svg" (arquivoSVG (desenhaBarras 0.0 (transforma(converte cod))))
   else
      putStr "Codigo INVALIDO\n"