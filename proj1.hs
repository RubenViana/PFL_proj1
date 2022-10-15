import Data.List
import Data.Char

--poly type -> [var, degree, coe] = [ord('x'), 2, 3] = "3*x^2"

--example poly

ex = [[ord 'x', 2, 0], [ord 'y', 1, 2], [ord 'z', 1, 5], [ord 'y', 1, 1], [ord 'y', 2, 7]]

sortPoly :: [[Int]] -> [[Int]]
sortPoly [] = []
sortPoly [x] = [x]
sortPoly (x:xs) = if (x !! 0 < (head xs) !! 0) then x : sortPoly xs else if (x !! 0 > (head xs) !! 0) then head xs : sortPoly (x : tail xs) else (if (x !! 1 < (head xs) !! 1) then head xs : sortPoly (x : tail xs) else x : sortPoly xs)


menu = do
        putStr "[1] Normalize polynomial\n[2] Add polynomials\n[3] Multiply polynomials\n[4] Derivate polynomial\n\n"
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

normalizePoly p = printPoly (norm (sortPoly p))

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