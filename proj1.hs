import Data.List
import Data.Char
import Data.List.Split


{-- 
    POLYNOMIAL DATA FUNCTONS
--}

data Poly = Poly {var :: String, coes :: [Float]} deriving (Show, Ord, Eq)

createPoly :: Float -> String -> Int -> Poly
createPoly c v d = Poly var (insertAt c d []) where var = if (d == 0) then "zzzz" else v


addTermToPolyList :: Float -> String -> Int -> [Poly] -> [Poly]
addTermToPolyList c v d pl 
                    |(isMember var (getAllVars pl)) = Poly var (updateCoes c d (getCoes (pl !! (head (elemIndices var (getAllVars pl)))))) : [p | p <- pl, getVar p /= var]
                    |otherwise = pl ++ [createPoly c var d]
                    where var = if (d == 0) then "zzzz" else v


addPolyToPolyList :: Poly -> [Poly] -> [Poly]
addPolyToPolyList p pl
                    |(isMember (getVar p) (getAllVars pl)) = Poly (getVar p) (zp (getCoes p) (getCoes (pl !! (head (elemIndices (getVar p) (getAllVars pl)))))) : [p1 | p1 <- pl, getVar p1 /= getVar p]
                    |otherwise = pl ++ [p]

getCoes :: Poly -> [Float]
getCoes (Poly _ coes) = coes

getVar :: Poly -> String
getVar (Poly var _) = var

getAllVars :: [Poly] -> [String]
getAllVars [] = []
getAllVars pl = map getVar pl

updateCoes :: Float -> Int -> [Float] -> [Float]
updateCoes n i c = zp (insertAt n i (take i (repeat 0))) c

stringListToPolyList :: [String] -> [Poly]
stringListToPolyList [] = []
stringListToPolyList (x:xs) = [toPoly x] ++ (stringListToPolyList xs)

stringToPolyList :: String -> [Poly]
stringToPolyList s = stringListToPolyList (splitString s)

takeCoe :: [Char] -> [Char]
takeCoe [x] = [x]
takeCoe x = takeWhile (/='*') x

takeVar :: [Char] -> [Char]
takeVar [x] = [x]
takeVar x = takeWhile (/='^') (dropWhile (\i -> not (isAlpha i)) x)

takeDeg :: [Char] -> [Char]
takeDeg x = let l = (dropWhile (/='^') x) in if ((length (l) > 0) && ((length x) /= 1)) then tail l else "1"

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
  | otherwise = createPoly 1 (takeVar (x:xs)) (read (takeDeg (x:xs))::Int) 

normRec :: [Poly] -> [Poly] -> [Poly]
normRec pl [] = pl
normRec pl (x:xs) = normRec (addPolyToPolyList x pl) xs

derivePoly :: [Poly] -> [Poly]
derivePoly [] = []
derivePoly (p:pl) = (concat[addTermToPolyList c (getVar p) (d) [] | c <- derive (getCoes p), c/=0, let d = head (elemIndices c (derive (getCoes p)))]) ++ derivePoly pl

showPoly pl = concat[concat[sig ++ show (abs c) ++ var ++ exp | c <- reverse (getCoes p), c /= 0, let sig = if (c >= 0) then " + " else " - ", let d = head (elemIndices c (getCoes p)), let var = if (d == 0) then "" else ("*" ++ getVar p), let exp = if (d <= 1) then "" else ("^" ++ show d)] | p <- sort pl]

{--
     AUX FUNCTIONS
--}

insertAt :: Float -> Int -> [Float] -> [Float]
insertAt ne i [] = (take i (repeat 0)) ++ [ne] 
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

derive :: [Float] -> [Float]
derive [] = []
derive (_:xs) = zipWith (*) xs [1..]

zp :: [Float] -> [Float] -> [Float]
zp a b = if (length a >= length b)
                then zipWith (+) a (b ++ repeat 0)
                else zp b a

isMember :: String -> [String] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

splitString :: String -> [String]
splitString s = split (startsWithOneOf ['+','-']) (dropSpaces s)

dropSpaces :: String -> String
dropSpaces s = filter (\x -> (x /= ' ')) s



{-- 
    MAIN FUNCTIONS
--}

normalizePolynomial :: [Poly] -> [Poly]
normalizePolynomial [] = []
normalizePolynomial pl = normRec [] pl

addPolynomials :: [Poly] -> [Poly] -> [Poly]
addPolynomials [] pl = pl
addPolynomials pl [] = pl
addPolynomials pl1 pl2 = normalizePolynomial (pl1 ++ pl2)

multiplyPolynomials :: [Poly] -> [Poly] -> [Poly]
multiplyPolynomials [] pl = pl
multiplyPolynomials pl [] = pl
--multiplyPolynomials pl1 pl2 = ...             TODO

derivePolynomial :: [Poly] -> [Poly]
derivePolynomial [] = []
derivePolynomial pl = normalizePolynomial (derivePoly pl)



{-- 
    MAIN MENU
--}

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
                putStrLn "(showPoly (multiplyPolynomials (stringToPolyList pol1) (stringToPolyList pol2)))" -- NOT IMPLEMENTED
        else if (opt == 4) then
            do
                putStr "Enter Polynomial: "
                pol <- getLine
                putStrLn (showPoly (derivePolynomial (stringToPolyList pol)))
        else if (opt == 0) then
            putStrLn "exit"
        else 
            main

-- example: 0*x^2 + 2*y -5*z + y + 7*y^2
