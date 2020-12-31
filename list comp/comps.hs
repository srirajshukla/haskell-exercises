import Data.Char
concat :: [[a]] -> [a]
concat xss = [x | xs <-xss, x<-xs]

firsts :: [(a, b)] -> [a]
firsts pairs = [x | (x, _) <- pairs]

-- length :: [a] -> Int
-- length xs = sum [1 | _ <-xs]


factors :: Int -> [Int]
factors n = [x | x<-[1..n], n`mod`x==0]

prime :: Int -> Bool
-- prime n = Prelude.length (factors n) == 2
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v)<-t, k==k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [pos|(val, pos)<-zip xs [0..], val==x]

positionsf ::Eq a => a -> [a] -> [Int]
positionsf x xs = find x (zip xs [0..]) 


lower :: String -> Int
lower xs = length [x|x<-xs, isAsciiLower x]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y)|x<-[0..m],y<-[0..n]]

square :: Int -> [(Int, Int)]
square m = [(a, b) | (a, b) <- grid m m, a/=b]

replicate :: Int -> a -> [a]
replicate n x = [x | _<-[1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x<-[1..n], y<-[1..n], z<-[1..n], (x^2 + y^2) == z^2]


perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == 2*x]


scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs1 xs2 = sum [ a*b|(a, b)<-zip xs1 xs2]
