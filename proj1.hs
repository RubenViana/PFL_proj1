import Data.List
import Data.Char

--poly type -> [var, degree, coe] = [ord('x'), 2, 3] = "3*x^2"

--example poly

--p1 = [[ord 'x', 2, 0], [ord 'y', 1, 2], [ord 'z', 1, 5], [ord 'y', 1, 1], [ord 'y', 2, 7]]
--p2 = [[ord 'z', 1, 10], [ord 'x', 2, 3]]

{--sortPoly :: [[Int]] -> [[Int]]
sortPoly [] = []
sortPoly [x] = [x]
sortPoly (x:y:xs)
                |(x !! 0 < y !! 0) = x : sortPoly (y:xs)
                |(x !! 0 > y !! 0) = y : sortPoly (x : xs)
                |(x !! 0 == y !! 0) && (x !! 1 >= y !! 1) = x : sortPoly (y : xs)
                |otherwise = y : sortPoly (x : xs)

completeSortPoly xs = iterate sortPoly xs !! length xs

menu = do
        putStr "[1] Normalize polynomial\n[2] Add polynomials\n[3] Multiply polynomials\n[4] Derive polynomial\n\n"
        putStr "Option: "
        str <- getLine
        putStr "\n"
        let opt = read str
        if (opt == 1) then
            putStr "Func format : normalizePoly [[ord 'var', degree, coe], ...]\n"
        else if (opt == 2) then
            putStr "not implemented\n"
        else if (opt == 3) then
            putStr "not implemented\n"
        else if (opt == 4) then
            putStr "not implemented\n"
        else
            putStr "exit\n"

normalizePoly p = printPoly (norm (completeSortPoly p))
addPoly p1 p2 = normalizePoly (addp (norm (completeSortPoly p1)) (norm (completeSortPoly p2)))
derivePoly p = normalizePoly (derive (norm (completeSortPoly p)))

norm :: [[Int]] -> [[Int]]
norm [] = []
norm [xs] = [xs]
norm (x:xs) 
            |((x !! 0) == (y !! 0)) && ((x !! 1) == (y !! 1)) = [x !! 0, x !! 1, (x !! 2) + (y !! 2)] : norm (tail xs)
            |x !! 2 == 0 = norm xs
            |otherwise = x : norm xs
            where y = head xs

printPoly :: [[Int]] -> String
printPoly [] = ""
printPoly p = tail(concat[op ++ show c ++ "*" ++ [(chr (v))] ++ deg| [v, d, c] <- sortPoly p, let op = if (c >= 0) then " +" else " ", let deg = if (d == 1) then "" else ("^" ++ show d)])


addp :: [[Int]] -> [[Int]] -> [[Int]]
addp p [] = p
addp p1 p2 = addp (p1 ++ [head p2]) (tail p2)


--derive a*x^i = a*i*x^(i-1)

derive :: [[Int]] -> [[Int]]
derive [] = []
derive (x:xs) = [x !! 0, (x !! 1) - 1, (x !! 2) * (x !! 1)] : derive (xs)


data Term = Term {coe :: Float, var :: Char, degree :: Int} deriving (Show)

getCoe :: Term -> Float
getCoe (Term coe _ _) = coe

getVar :: Term -> Char
getVar (Term _ var _) = var

getDeg :: Term -> Int
getDeg (Term _ _ deg) = deg

createTerm :: Float -> Char -> Int -> Term
createTerm c v d = Term c v d

x = createTerm 3 'x' 2
y = createTerm 4 'y' 1
z = createTerm 2 'y' 1

polyList = [x,y,z] --}

data Poly = Poly {var :: String, coes :: [Float]} deriving (Show, Ord, Eq)

createPoly :: Float -> String -> Int -> Poly
createPoly c v d = Poly v (insertAt c d [])

addToPolyList :: Float -> String -> Int -> [Poly] -> [Poly]
addToPolyList c v d [] = [createPoly c v d] 
addToPolyList c v d pl 
                    |isMember v (getAllVars pl) = Poly v (updateCoes c d (getCoes (pl !! (head (elemIndices v (getAllVars pl)))))) : [p | p <- pl, getVar p /= v]
                    |otherwise = pl ++ [createPoly c v d]

getCoes :: Poly -> [Float]
getCoes (Poly _ coes) = coes

getVar :: Poly -> String
getVar (Poly var _) = var

getAllVars :: [Poly] -> [String]
getAllVars [] = []
getAllVars pl = map getVar pl

updateCoes :: Float -> Int -> [Float] -> [Float]
updateCoes n i c = zp (insertAt n i (take i (repeat 0))) c

stringToListPolys :: String -> [Poly]
stringToListPolys "" = []
--stringToListPolys -- '+/-/ ' -> int -> '*' -> char -> '^' -> int  <<== maybe state machine! or create a func for each type o value...

addPolys :: [Poly] -> [Poly] -> [Poly]
addPolys [pl] _ = [pl]
addPolys pl1 [] = pl1
addPolys pl1 (p2:pl2) = addPolys (concat[addToPolyList c (getVar p2) d pl1 |  c <- getCoes p2, c /= 0, let d = head (elemIndices c (getCoes p2))]) pl2


showPoly pl = concat[concat[sig ++ show (abs c) ++ "*" ++ getVar p ++ exp | c <- reverse (getCoes p), c /= 0, let sig = if (c >= 0) then " + " else " - ", let x = head (elemIndices c (getCoes p)), let exp = if (x == 1) then "" else ("^" ++ show x)] | p <- sort pl]

insertAt :: Float -> Int -> [Float] -> [Float]
insertAt ne i [] = (take i (repeat 0)) ++ [ne] 
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as


zp :: [Float] -> [Float] -> [Float]
zp a b = if (length a >= length b)
                then zipWith (+) a (b ++ repeat 0)
                else zp b a

isMember :: String -> [String] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs


-- example: 0*x^2 + 2*y + 5*z + y + 7*y^2
example :: [Poly]
example = [createPoly 0 "x" 2, createPoly 2 "y" 1, createPoly 5 "z" 1, createPoly 1 "y" 1, createPoly 7 "y" 2]


pp1 = addToPolyList (-7) "y" 2 (addToPolyList 1 "y" 1 (addToPolyList 5 "z" 1 (addToPolyList 2 "y" 1 (addToPolyList 0 "x" 2 []))))
pp2 = addToPolyList 5 "x" 2 (addToPolyList (3) "y" 2 [])


