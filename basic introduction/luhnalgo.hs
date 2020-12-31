luhnDouble :: Int -> Int 
luhnDouble x | 2*x>9 = 2*x-9
             | otherwise = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn a b c d = n`mod`10 == 0
    where
        n = luhnDouble a + b + luhnDouble c + d
