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

normalizePoly p = printPoly (norm (completeSortPoly p))
addPoly p1 p2 = normalizePoly (addp (norm (completeSortPoly p1)) (norm (completeSortPoly p2)))

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


