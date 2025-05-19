ones :: [Int]
ones = 1 : ones

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n - 1) xs

myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

myCycle :: [a] -> [a]
myCycle l = helper l l
    where 
        helper l [] = helper l l
        helper l (x:xs) = x : helper l xs

myIterate :: (a -> a) -> a -> [a]
myIterate f x = helper f x
    where
        helper f x = f x : helper f (f x)


main :: IO()
main = do
    let res = myTake 10 (myIterate (*2) 1)
    putStrLn ("Answer is " ++ show res)