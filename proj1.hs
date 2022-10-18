import Data.List
import Data.Char

--poly type -> [var, degree, coe] = [ord('x'), 2, 3] = "3*x^2"

--example poly

p1 = [[ord 'x', 2, 0], [ord 'y', 1, 2], [ord 'z', 1, 5], [ord 'y', 1, 1], [ord 'y', 2, 7]]
p2 = [[ord 'z', 1, 10], [ord 'x', 2, 3]]

sortPoly :: [[Int]] -> [[Int]]
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


{--data Term = Term {coe :: Float, var :: Char, degree :: Int} deriving (Show)

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
--addToPolyList c v d pl 
--                    |v in pl.getAllVars = insertAt c (d - 1) (getCoes plv) --pseudocode
--                    |otherwise = pl ++ [createPoly c v d]

getCoes :: Poly -> [Float]
getCoes (Poly _ coes) = coes

getVar :: Poly -> String
getVar (Poly var _) = var

getAllVars :: [Poly] -> [String]
getAllVars [] = []
--getAllVars pl = filter (\getVar _ -> var) pl

stringToListPolys :: String -> [Poly]
stringToListPolys "" = []
--stringToListPolys -- '+/-/ ' -> int -> '*' -> char -> '^' -> int  <<== maybe state machine! or create a func for each type o value...

showPoly pl = concat[concat[" + " ++ show c ++ "*" ++ getVar p ++ "^" ++ show (length (getCoes p)) | c <- reverse (getCoes p), c > 0] | p <- sort pl]

insertAt :: Float -> Int -> [Float] -> [Float]
insertAt ne i [] = (take i (repeat 0)) ++ [ne] 
insertAt newElement i (a:as)
  | length(as)+1 < i = insertAt newElement i (zp (a:as) (take i (repeat 0)))
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

zp :: [Float] -> [Float] -> [Float]
zp a b = if (length a >= length b)
                then zipWith (+) a (b ++ repeat 0)
                else zp b a

a = [0,1,2,3]
b = [0,0,0,0,0,0,0,0]

-- example: 0*x^2 + 2*y + 5*z + y + 7*y^2
example :: [Poly]
example = [createPoly 0 "x" 2, createPoly 2 "y" 1, createPoly 5 "z" 1, createPoly 1 "y" 1, createPoly 7 "y" 2]

