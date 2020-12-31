(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs Main.++ ys)


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <=y = x:y:ys
                | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : Main.zip xs ys

fib :: Int -> Int 
-- fib n | n <=1 = n
--       | otherwise = fib (n-1) + fib (n-2)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


products :: Num a => [a] -> a
products [] = 1
products (x:xs) = x*products xs

drops :: Integral b => b -> [a] -> [a]
drops 0 xs = xs
drops _ [] =  []
drops n (_:xs) = drops (n-1) xs 

inits :: [a] -> [a]
inits (x:xs) | null xs = []
             | otherwise = x: inits xs


fact :: Integer  -> Integer 
fact n | n<1 = 0
       | n==1 = 1
       | otherwise = n * fact (n-1)
-- fact 1 = 1
-- fact n = n * fact (n-1)

sumdown :: Int -> Int 
sumdown 0 = 0
sumdown i = i + sumdown(i-1)

(^) :: Int -> Int -> Int 
(^) a 0 = 1
(^) a b = a*(a Main.^ (b-1)) 

euclid'sGCD :: Int -> Int -> Int 
euclid'sGCD a b | a==b = a
                | otherwise = euclid'sGCD (mx-mn) mn
                where
                    mx = max a b
                    mn = min a b   

euclid'sGCD2 :: Int -> Int -> Int
euclid'sGCD2 a b | a==b = a
                 | a > b = euclid'sGCD2 (a-b) b
                 | b > a = euclid'sGCD2 a (b-a)


lengths :: [a] -> Int 
lengths [] = 0
lengths (_:xs) = 1 + lengths xs

ands :: [Bool] -> Bool 
ands [] = True
ands [x] = x
ands (x:xs) = x && ands xs

concats :: [[a]] -> [a]
concats [x] = x
concats (x : xs) = x Main.++ concats xs

replicates :: Int -> a -> [a]
replicates 1 x = [x]
replicates n x = x : replicates (n-1) x

(!!) :: [a] -> Int -> a
-- returns ith value of list, a[Int] == [a] !! Int
(!!) xs 0 = head xs
(!!) xs n = (Main.!!) (tail xs) (n-1)


elem :: Eq a => a -> [a] -> Bool 
elem _ [] = False 
elem y [x] = x==y
elem y (x:xs) =  y == x || Main.elem y xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) = if a<=b
        then a:merge as (b:bs)
        else b:merge (a:as) bs

halve :: [a] -> ([a], [a])
halve a = splitAt (length a`div`2) a

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort as = merge (msort (fst (halve as))) (msort (snd (halve as)))

sumoflist :: Num a => [a] -> a
sumoflist [] = 0
sumoflist xs = head xs + sumoflist (tail xs)

taken :: Int -> [a] -> [a]
taken 0 _ = []
taken n [] = []
taken n (x:xs) = x:taken (n-1) xs

selectlast :: [a] -> a
selectlast [a] = a
selectlast (a:as) = selectlast as