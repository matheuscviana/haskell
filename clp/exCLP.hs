-- Resposta da lista de exerc�cios (vers�o Prolog)

-- 1. Implemente uma fun��o/regra que verifica
--    se um elemento pertence a uma lista.

pertence x [] = False
pertence x (a:b) = if a == x then True else pertence x b

--essa quest�o equivale � fun��o elem do haskell 

pertence2 x lista = elem x lista


-- 2. Implemente uma fun��o/regra que inverte
--    os elementos de uma lista.

inverte [] = []
inverte (a:b) = inverte b ++ [a]

--essa quest�o equivale � fun��o reverse do haskell 

inverte2 lista = reverse lista


-- 3. Implemente uma fun��o/regra que informe
--    o n-�simo membro de uma lista.

n_esimo 0 (a:_) = a 
n_esimo i (a:b) = n_esimo (i-1) b

--essa quest�o equivale ao operador !! do haskell 

n_esimo2 x lista = lista !! x

-- 4. Implemente uma fun��o/regra que remova
--    todas as ocorr�ncias de um elemento em uma lista.

retiraTodos a lst = [x | x <- lst, x /= a]

retiraTodos2 a lst = filter (/=a) lst


-- 5. Implemente uma fun��o/regra que recebe 
--    uma lista e a divide em duas pela metade.

divide lista = (take metade lista, drop metade lista)
   where
      metade = div (length lista) 2
 

-- 6. Implemente uma fun��o/regra que verifica se uma String
--    � ou n�o um pal�ndromo. Um pal�ndromo � uma String que
--    se invertida se mant�m exatamente igual � original.

palindromo lista = lista == (inverte lista)   


-- 7. Implemente uma fun��o/regra primo(N) que dado um n�mero
--    inteiro N, determine se ele � ou n�o um n�mero primo.
--    round arredonda um float e sqrt retorna a raiz de um n�mero.
--OBS.: a maioria dos autores falam que 1 n�o � primo

primo :: Integer -> Bool
primo 0 = False
primo 1 = False
primo 2 = True
primo x = primo_aux x (truncate (sqrt (fromIntegral x)))

primo_aux :: Integer -> Integer -> Bool 
primo_aux _ 1 = True
primo_aux x d = if mod x d == 0 then False else primo_aux x (d-1)   


-- 8. Implemente uma fun��o/regra lista_primos(N, Lista) que
--    aceite um n�mero inteiro e que determine a lista de todos
--    os n�meros primos iguais ou inferiores a esse n�mero.


lista_primos n
  | n < 2     = []  
  | primo(n)  = lista_primos (n-1) ++ [n]
  | otherwise = lista_primos (n-1) 


-- 9. Escreva uma fun��o/regra chamada conceito que receba
--    uma nota como argumento e retorne o conceito
--    correspondente, conforme as regras:  nota >= 9.0,
--    conceito A; nota >= 7.5 e < 9.0, conceito B; nota >= 6.0
--    e < 7.5, conceito C; nota >= 4.0 e < 6.0, conceito D;
--    nota < 4.0, conceito E.

conceito nota 
  | nota >= 9.0 = 'A'
  | nota >= 7.5 && nota < 9.0 = 'B'
  | nota >= 6.0 && nota < 7.5 = 'C'
  | nota >= 4.0 && nota < 6.0 = 'D'
  | nota < 4.0 = 'E'


-- 10. Implemente o algoritmo de ordena��o mergesort.

mergesort []  = []
mergesort [x] = [x]
mergesort lst = conquistar (mergesort me) (mergesort md)
  where (me,md) = divide lst

conquistar [] lst = lst
conquistar lst [] = lst
conquistar (p1:r1) (p2:r2)
  | p1 < p2   = p1 : conquistar r1 (p2:r2)
  | otherwise = p2 : conquistar (p1:r1) r2 
