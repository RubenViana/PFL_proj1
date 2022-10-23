import Data.List
import Data.Char
import Data.List.Split


{-- 
    POLYNOMIAL DATA FUNCTONS
--}

-- Internal Representation:

data Poly = Poly {var :: String, coes :: [Float]} deriving (Show, Ord, Eq)

-- Create a polynomial:

createPoly :: Float -> String -> Int -> Poly
createPoly c v d = Poly var (insertAt c d []) where var = if (d == 0) then "zzzz" else v

-- Add a term to a polynomial list:

addTermToPolyList :: Float -> String -> Int -> [Poly] -> [Poly]
addTermToPolyList c v d pl 
                    |(isMember var (getAllVars pl)) = Poly var (updateCoes c d (getCoes (pl !! (head (elemIndices var (getAllVars pl)))))) : [p | p <- pl, getVar p /= var]
                    |otherwise = pl ++ [createPoly c var d]
                    where var = if (d == 0) then "zzzz" else v

-- Add a polynomial to a polynomial list

addPolyToPolyList :: Poly -> [Poly] -> [Poly]
addPolyToPolyList p pl
                    |(isMember (getVar p) (getAllVars pl)) = Poly (getVar p) (zp (getCoes p) (getCoes (pl !! (head (elemIndices (getVar p) (getAllVars pl)))))) : [p1 | p1 <- pl, getVar p1 /= getVar p]
                    |otherwise = pl ++ [p]

-- Return a list of coefficients

getCoes :: Poly -> [Float]
getCoes (Poly _ coes) = coes

-- Return a variable

getVar :: Poly -> String
getVar (Poly var _) = var

-- Return a list of all the variables of a polynomial list

getAllVars :: [Poly] -> [String]
getAllVars [] = []
getAllVars pl = map getVar pl

-- Update the coefficients list

updateCoes :: Float -> Int -> [Float] -> [Float]
updateCoes n i c = zp (insertAt n i (take i (repeat 0))) c

-- Convert a list of strings to a list of polynomials

stringListToPolyList :: [String] -> [Poly]
stringListToPolyList [] = []
stringListToPolyList (x:xs) = [toPoly x] ++ (stringListToPolyList xs)

-- Convert a string to a list of polynomials

stringToPolyList :: String -> [Poly]
stringToPolyList s = stringListToPolyList (splitString s)

-- Select coefficients

takeCoe :: [Char] -> [Char]
takeCoe [x] = [x]
takeCoe x = takeWhile (/='*') x

-- Select variables

takeVar :: [Char] -> [Char]
takeVar [x] = [x]
takeVar x = takeWhile (/='^') (dropWhile (\i -> not (isAlpha i)) x)

-- Select degrees

takeDeg :: [Char] -> [Char]
takeDeg x = let l = (dropWhile (/='^') x) in if ((length (l) > 0) && ((length x) /= 1)) then tail l else "1"

-- Convert a string into a polynomial

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
  
-- Recursive normalization  

normRec :: [Poly] -> [Poly] -> [Poly]
normRec pl [] = pl
normRec pl (x:xs) = normRec (addPolyToPolyList x pl) xs

-- Polynomial derivation

derivePoly :: [Poly] -> [Poly]
derivePoly [] = []
derivePoly (p:pl) = (concat[addTermToPolyList c (getVar p) (d) [] | c <- derive (getCoes p), c/=0, let d = head (elemIndices c (derive (getCoes p)))]) ++ derivePoly pl

-- Convert a polynomial list into a string

showPoly :: [Poly] -> String
showPoly pl = concat[concat[sig ++ show (abs c) ++ var ++ exp | c <- reverse (getCoes p), c /= 0, let sig = if (c >= 0) then " + " else " - ", let d = head (elemIndices c (getCoes p)), let var = if (d == 0) then "" else ("*" ++ getVar p), let exp = if (d <= 1) then "" else ("^" ++ show d)] | p <- sort pl]

{--
     AUX FUNCTIONS
--}

-- Insert an element at a given position in a list

insertAt :: Float -> Int -> [Float] -> [Float]
insertAt ne i [] = (take i (repeat 0)) ++ [ne] 
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

-- Derivation of a float list

derive :: [Float] -> [Float]
derive [] = []
derive (_:xs) = zipWith (*) xs [1..]

-- Zip two lists with different sizes 

zp :: [Float] -> [Float] -> [Float]
zp a b = if (length a >= length b)
                then zipWith (+) a (b ++ repeat 0)
                else zp b a

-- Verify if a given string belongs to a list

isMember :: String -> [String] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

-- Splits a string by +/- operators

splitString :: String -> [String]
splitString s = split (startsWithOneOf ['+','-']) (dropSpaces s)

-- Eliminates white spaces in a string

dropSpaces :: String -> String
dropSpaces s = filter (\x -> (x /= ' ')) s

{-- 
    MAIN FUNCTIONS
--}

-- Functions already commented in README

normalizePolynomial :: [Poly] -> [Poly]
normalizePolynomial [] = []
normalizePolynomial pl = normRec [] pl

addPolynomials :: [Poly] -> [Poly] -> [Poly]
addPolynomials [] pl = pl
addPolynomials pl [] = pl
addPolynomials pl1 pl2 = normalizePolynomial (pl1 ++ pl2)

multiplyPolynomials :: [Poly] -> [Poly] -> [Poly]
multiplyPolynomials [] pl = []
multiplyPolynomials pl [] = []
multiplyPolynomials pl1 pl2 = concat[ concat[ addTermToPolyList (c1*c2) (v1 ++ if (v1 == "" || v2 == "")then "" else "*" ++ v2) (1) [] | p2 <- pl2, c2 <- getCoes p2, c1 <- getCoes p1, c1 /= 0, c2 /= 0, let v1 = if (head (elemIndices c1 (getCoes p1)) == 1 && getVar p1 /= "zzzz") then getVar p1 else if (getVar p1 == "zzzz") then "" else getVar p1 ++ "^" ++ show(head (elemIndices c1 (getCoes p1))), let v2 = if (head (elemIndices c2 (getCoes p2)) == 1 && getVar p2 /= "zzzz") then getVar p2 else if (getVar p2 == "zzzz") then "" else getVar p2 ++ "^" ++ show(head (elemIndices c2 (getCoes p2)))] | p1 <- pl1]


derivePolynomial :: [Poly] -> [Poly]
derivePolynomial [] = []
derivePolynomial pl = normalizePolynomial (derivePoly pl)



{-- 
    MAIN MENU
--}

-- Already commented in README

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

-- example: 0*x^2 + 2*y -5*z + y + 7*y^2

p1 = stringToPolyList "x^2 + x"
p2 = stringToPolyList "x + 5"



