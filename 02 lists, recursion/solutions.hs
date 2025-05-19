import Distribution.Backpack (dispOpenModuleSubst)
import Data.IntMap (difference)
import Distribution.Simple.Utils (xargs)
import Data.Text.Array (run)
isElement :: (Eq a) => a -> [a] -> Bool
isElement _ [] = False
isElement x (y:xs)
    | x == y = True
    | x /= y = isElement x xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myMap :: (a -> a) -> [a] -> [a]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = myFilter f xs
    | otherwise = x : myFilter f xs

myFoldr :: (a -> a -> a) -> a -> [a] -> a
myFoldr _ nullValue [] = nullValue
myFoldr op nullValue (x:xs) = op x (myFoldr op nullValue xs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort left) (sort right)
    where
        (left, right) = splitAt (length xs `div` 2) xs 

getAtIndex :: Int -> [a] -> a
getAtIndex 1 (x:xs) = x
getAtIndex i (x:xs) = getAtIndex (i - 1) xs

column :: Int -> [[a]] -> [a]
column _ [] = []
column i (x:xs) = getAtIndex i x : column i xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = firstColumn rows : transpose(lastColumns rows)
    where
        firstColumn [] = []
        firstColumn (x:xs) = head x : firstColumn xs

        lastColumns [] = []
        lastColumns (x:xs) = tail x : lastColumns xs

diagonal :: [[a]] -> [a]
diagonal xs = diagonalRec 1 xs
    where 
        diagonalRec _ [] = []
        diagonalRec i (x:xs) = getAtIndex i x : diagonalRec (i + 1) xs

foldm :: (a -> a -> a) -> a -> [[a]] -> a
foldm _ zero [] = zero
foldm op zero (x:xs) = myFoldr op zero x `op` foldm op zero xs

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) ys 
    | isElement x ys = x : intersection xs ys
    | otherwise      = intersection xs ys

myDifference :: (Eq a) => [a] -> [a] -> [a]
myDifference xs [] = xs
myDifference [] _ = []
myDifference (x:xs) ys
    | not $ isElement x ys = x : myDifference xs ys
    | otherwise            = myDifference xs ys

dotProduct :: [Int] -> [Int] -> Int
dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys

triplets :: Int -> [(Int, Int, Int)]
triplets n = [(a, b, c) | c <- [1..n], b <- [1..c], a <-[1..b], a^2 + b^2 == c^2, a + b + c <= 30]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys) 
    | x == y    = remove x ys
    | otherwise = y : remove x ys

myCount :: (Eq a) => a -> [a] -> Int
myCount x xs = myCountRec 0 x xs 
    where
        myCountRec n _ [] = n 
        myCountRec n x (y:ys) 
            | x == y    = myCountRec (n + 1) x ys
            | otherwise = myCountRec n x ys

histogram :: (Eq a) => [a] -> [(a, Int)]
histogram [] = []
histogram (x:xs) = (x, myCount x (x:xs) ) : histogram (remove x xs)

runLengthEncode :: (Eq a) => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (x:xs) = helper x 1 xs
    where
        helper curr n [] = [(curr, n)]
        helper curr n (y:ys) 
            | curr == y = helper curr (n + 1) ys
            | otherwise = (curr, n) : helper y 1 ys
 
main :: IO()
main = do
    let res = runLengthEncode [8, 7, 7, 7, 7, 1, 1, 1, 1, 1, 7, 8, 2, 2, 8, 2, 7, 8, 1]
    putStrLn ("Answer is: " ++ show res)
