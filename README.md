# PFL_TP1_#G03_12.zip


## Representação interna de polinómios:

- *Representação interna*.
- *Code*:

```
data Poly = Poly {var :: String, coes :: [Float]} deriving (Show, Ord, Eq)
```

### Descrição da escolha:

- Considerando os polinómios, fez-se uma repartição em variáveis do tipo `string` e coeficientes do tipo `float`.

### Justificação da escolha:

- Recorrendo a conhecimentos matemáticos prévios e ao pensamento na altura de início do projeto pareceu-nos uma das formas mais fáceis e eficientes de representar o polinómio.

## Funcionalidades e descrição da estratégia de implementação de cada uma:

### Funções principais:

- *normalizePolynomial*: Função que realiza a normalização dos polinómios, ou seja, coloca-os na forma normal.
- *Descrição de estratégia*: Iterar a lista do polinómios de forma recursiva e ao comparar dois polinómios se tivessem a mesma variável juntam-se as listas dos dois, caso contrário mantinham-se os polinómios iguais.
-*Code*:

```
normalizePolynomial :: [Poly] -> [Poly]
normalizePolynomial [] = []
normalizePolynomial pl = normRec [] pl
```

- *addPolynomials*: Função que realiza a adição dos polinómios.
- *Descrição de estratégia*: Fazer a normalização da concatenação dos dois polinómios.
- *Code*:

```
addPolynomials :: [Poly] -> [Poly] -> [Poly]
addPolynomials [] pl = pl
addPolynomials pl [] = pl
addPolynomials pl1 pl2 = normalizePolynomial (pl1 ++ pl2)
```

- *multiplyPolynomials*: Função que realiza a multiplicação dos polinómios.
- *Descrição de estratégia*: Para cada elemento do primeiro polinómio multiplicar com todos os elementos do segundo polinómio.
- *Code*:

```
multiplyPolynomials :: [Poly] -> [Poly] -> [Poly]
multiplyPolynomials [] _ = [0]
multiplyPolynomials _ [] = [0]
multiplyPolynomials pl1 pl2 = normalizePolynomial (multiply pl1 pl2) 
```

- *derivePolynomial*: Função que realiza a derivação dos polinómios.
- *Descrição de estratégia*: Tendo um polinómio, pega-se na cauda da lista dos coeficientes deste e multiplica-se pelo seu indíce mais um.
- *Code*:

```
derivePolynomial :: [Poly] -> [Poly]
derivePolynomial [] = []
derivePolynomial pl = normalizePolynomial (derivePoly pl)
```

### Funções auxiliares:

- *insertAt*: Função que permite inserir um elemento numa dada posição.
*Descrição de estratégia*:
*Code*:

```
insertAt :: Float -> Int -> [Float] -> [Float]
insertAt ne i [] = (take i (repeat 0)) ++ [ne]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as
```

- *derive*: Função que faz a efetiva derivação.
*Descrição de estratégia*: Tendo uma lista, pega-se na cauda da lista dos coeficientes e multiplica-se pelo seu indíce mais um.
*Code*:

```
derive :: [Float] -> [Float]
derive [] = []
derive (_:xs) = zipWith (*) xs [1..]
```

- *multiply*: Função que faz a efetiva multiplicação de polinómios.
*Descrição de estratégia*:
*Code*:

```
multiply :: [Float] -> [Float] -> Float
multiply [] _  = [0]
multiply (x : xs) ys = add (map (*x) ys) (0 : (multiply (xs) ys))
```

- *zp*: Função que faz.
*Descrição de estratégia*:
*Code*:

```
zp :: [Float] -> [Float] -> [Float]
zp a b = if (length a >= length b)
                then zipWith (+) a (b ++ repeat 0)
                else zp b a
```

- *isMember*: Função que verifica se dado elemento é faz parte da `string`,
*Descrição de estratégia*:
*Code*:

```
isMember :: String -> [String] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs
```

### Funções auxiliares ("DATA"):

- *splitString*: Função que permite separar as `strings`.
*Descrição de estratégia*:
*Code*:

```
splitString :: String -> [String]
splitString s = split (startsWithOneOf ['+','-']) (dropSpaces s)
```

- *dropSpaces*: Função que permite retirar os espaçamentos.
*Descrição de estratégia*:
*Code*:

```
dropSpaces :: String -> String
dropSpaces s = filter (\x -> (x /= ' ')) s
```

- *Representação interna*.
*Code*:

```
data Poly = Poly {var :: String, coes :: [Float]} deriving (Show, Ord, Eq)
```

- *createPoly*: Função que permite criar um polinómio.
*Descrição de estratégia*:
*Code*:

```
createPoly :: Float -> String -> Int -> Poly
createPoly c v d = Poly var (insertAt c d []) where var = if (d == 0) then "zzzz" else v
```

- *addTermToPolyList*: Função que adiciona um termo à lista de polinómios.
*Descrição de estratégia*:
*Code*:

```
addTermToPolyList :: Float -> String -> Int -> [Poly] -> [Poly]
addTermToPolyList c v d pl
                    |(isMember var (getAllVars pl)) = Poly var (updateCoes c d (getCoes (pl !! (head (elemIndices var (getAllVars pl)))))) : [p | p <- pl, getVar p /= var]
                    |otherwise = pl ++ [createPoly c var d]
                    where var = if (d == 0) then "zzzz" else v
```

- *addPolyToPolyList*: Função que adiciona um polinómio à lista de polinómios.
*Descrição de estratégia*:
*Code*:

```
addPolyToPolyList :: Poly -> [Poly] -> [Poly]
addPolyToPolyList p pl
                    |(isMember (getVar p) (getAllVars pl)) = Poly (getVar p) (zp (getCoes p) (getCoes (pl !! (head (elemIndices (getVar p) (getAllVars pl)))))) : [p1 | p1 <- pl, getVar p1 /= getVar p]
                    |otherwise = pl ++ [p]
```

- *getCoes*: Função que recebe os coeficientes.
*Descrição de estratégia*:
*Code*:

```
getCoes :: Poly -> [Float]
getCoes (Poly _ coes) = coes
```

- *getVar*: Função que recebe as variáveis.
*Descrição de estratégia*:
*Code*:

```
getVar :: Poly -> String
getVar (Poly var _) = var
```

- *getAllVars*: Função que recebe todas as variáveis.
*Descrição de estratégia*:
*Code*:

```
getAllVars :: [Poly] -> [String]
getAllVars [] = []
getAllVars pl = map getVar pl
```

- *updateCoes*: Função que atualiza os coeficientes.
*Descrição de estratégia*:
*Code*:

```
updateCoes :: Float -> Int -> [Float] -> [Float]
updateCoes n i c = zp (insertAt n i (take i (repeat 0))) c
```

- *stringListToPolyList*: Função que passa uma lista de `strings` para uma lista de `polinómios`.
*Descrição de estratégia*:
*Code*:

```
stringListToPolyList :: [String] -> [Poly]
stringListToPolyList [] = []
stringListToPolyList (x:xs) = [toPoly x] ++ (stringListToPolyList xs)
```

- *stringToPolyList*: Função que passa uma `string` para uma lista de `polinómios`.
*Descrição de estratégia*:
*Code*:

```
stringToPolyList :: String -> [Poly]
stringToPolyList s = stringListToPolyList (splitString s)
```

- *takeCoe*: Função que apanha os coeficientes.
*Descrição de estratégia*:
*Code*:

```
takeCoe :: [Char] -> [Char]
takeCoe [x] = [x]
takeCoe x = takeWhile (/='*') x
```

- *takeVar*:  Função que apanha as variáveis.
*Descrição de estratégia*:
*Code*:

```
takeVar :: [Char] -> [Char]
takeVar [x] = [x]
takeVar x = takeWhile (/='^') (dropWhile (\i -> not (isAlpha i)) x)
```

- *takeDeg*: Função que apanha os graus.
*Descrição de estratégia*:
*Code*:

```
takeDeg :: [Char] -> [Char]
takeDeg x = let l = (dropWhile (/='^') x) in if ((length (l) > 0) && ((length x) /= 1)) then tail l else "1"
```

- *toPoly*: Função que passa para a `string` para polinómio.
*Descrição de estratégia*:
*Code*:

```
toPoly :: String -> Poly
toPoly [] = createPoly 0 " " 0
toPoly (x:xs)
  | ((x == '+') && not('*' `elem` xs) && isDigit(head xs)) = createPoly (read(xs)::Float) " " 0
  | ((x == '-') && not('*' `elem` xs) && isDigit(head xs)) = createPoly (negate (read(xs)::Float)) " " 0
  | (not('*' `elem` (x:xs)) && isDigit(x)) = createPoly (read(x:xs)::Float) " " 0
  | (x == '+' && isDigit(head xs)) = createPoly (read (takeCoe xs)::Float) (takeVar xs) (read (takeDeg xs)::Int)
  | (x == '-' && isDigit(head xs)) = createPoly (negate (read (takeCoe xs)::Float)) (takeVar xs) (read (takeDeg xs)::Int)
  | (isDigit(x)) = createPoly (read (takeCoe (x:xs))::Float) (takeVar (x:xs)) (read (takeDeg (x:xs))::Int)
  | (x == '+') = createPoly 1 (takeVar xs) (read (takeDeg xs)::Int)
  | (x == '-') = createPoly (-1) (takeVar xs) (read (takeDeg xs)::Int)
  | otherwise = createPoly 1 (takeVar (x:xs)) (read (takeDeg (x:xs))::Int)`
```

- *normRec*: Função que normaliza efetivamente os polinómios.
*Descrição de estratégia*:
*Code*:

```
normRec :: [Poly] -> [Poly] -> [Poly]
normRec pl [] = pl
normRec pl (x:xs) = normRec (addPolyToPolyList x pl) xs
```

- *derivePoly*: Função que usa a derivação e a vai fazendo ao longo do polinómio.
*Descrição de estratégia*:
*Code*:

```
derivePoly :: [Poly] -> [Poly]
derivePoly [] = []
derivePoly (p:pl) = (concat[addTermToPolyList c (getVar p) (d) [] | c <- derive (getCoes p), c/=0, let d = head (elemIndices c (derive (getCoes p)))]) ++ derivePoly pl
```

- *showPoly*: Função que mostra o polinómio.
*Descrição de estratégia*:
*Code*:

```
showPoly pl = concat[concat[sig ++ show (abs c) ++ var ++ exp | c <- reverse (getCoes p), c /= 0, let sig = if (c >= 0) then " + " else " - ", let d = head (elemIndices c (getCoes p)), let var = if (d == 0) then "" else ("*" ++ getVar p), let exp = if (d <= 1) then "" else ("^" ++ show d)] | p <- sort pl]
```




## Exemplos de utilização que permitam testar todas as funcionalidades do programa:

- Chamar a função `main`, intoduzir o número correspondente à opção desejada como descrito abaixo.

## Adicional:

- *menu*: Realizou-se um menu para facilitar a interação do utilizador com o programa.
*Descrição de estratégia*: Está repartido por quatro opções de trabalho (*1 - Normalizar polinómios*, *2 - Adicionar polinómios*, *3 - Multiplicar polinómios*, *4 - Derivar polinómios*) e uma opção de saída (*0 - exit*)

```
main = do
        putStr "[1] Normalize polynomial\n[2] Add polynomials\n[3] Multiply polynomials\n[4] Derive polynomial\n[0] Exit\n\n"
        putStr "Option: "
        str <- getLine
        putStr "\n"
        let opt = read str
        if (opt == 1) then
            do
                putStr "Enter Polynomial: "
                pol <- getLine
                putStrLn (showPoly (normalizePolynomial (stringToPolyList pol)))
        else if (opt == 2) then
            do
                putStr "Enter Polynomial 1: "
                pol1 <- getLine
                putStr "Enter Polynomial 2: "
                pol2 <- getLine
                putStrLn (showPoly (addPolynomials (stringToPolyList pol1) (stringToPolyList pol2)))
        else if (opt == 3) then
            do
                putStr "Enter Polynomial 1: "
                pol1 <- getLine
                putStr "Enter Polynomial 2: "
                pol2 <- getLine
                putStrLn (showPoly (multiplyPolynomials (stringToPolyList pol1) (stringToPolyList pol2)))
        else if (opt == 4) then
            do
                putStr "Enter Polynomial: "
                pol <- getLine
                putStrLn (showPoly (derivePolynomial (stringToPolyList pol)))
        else if (opt == 0) then
            putStrLn "exit"
        else
            main
```
