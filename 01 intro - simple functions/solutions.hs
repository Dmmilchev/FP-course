import Distribution.Simple.Utils (xargs)
add :: Int -> Int -> Int
add x y = x + y

addN :: Num Int => Int -> Int -> Int
addN = (+)

signum1 :: (Ord a, Num a) => a -> a
signum1 x 
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

factorial :: Int -> Int
factorial x
    | x <= 1 = 1
    | otherwise = x * factorial(x - 1)

fibonacci :: Int -> Int
fibonacci x
    | x <= 2 = 1
    | otherwise = fibonacci(x - 1) + fibonacci(x - 2)  -- not the most efficient

mySum :: Int -> Int -> Int
mySum x y
    | y < x = 0
    | x == y = x
    | otherwise = x + mySum(x + 1) y

fastPow :: Float -> Int -> Float
fastPow _ 0 = 1
fastPow x n 
    | even n = fastPow (x * x) (div n 2)
    | odd n  = x * fastPow (x * x) (div (n - 1) 2)

countDigits :: Int -> Int
countDigits 0 = 0
countDigits n = 1 + countDigits (div n 10)

twice :: (a -> a) -> a -> a
twice f x = f $ f x

myCompose :: (a -> a) -> (a -> a) -> (a -> a)
myCompose f g = f . g

repeated :: (a -> a) -> Int -> (a -> a)
repeated f 1 = f
repeated f n = f . repeated f (n - 1)

myAccumulate :: (Ord a) => (a -> a -> a) -> a -> (a -> a) -> a -> (a -> a) -> a -> a
myAccumulate combiner nullValue term start next end 
    | start > end = nullValue
    | otherwise = combiner (term start) (myAccumulate combiner nullValue term (next start) next end)

main :: IO()
main = do
    let res = myAccumulate (+) 0 (*2) 0 (+10) 10
    putStrLn ("Answer is " ++ show res)