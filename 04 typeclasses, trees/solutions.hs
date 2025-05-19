import Control.Arrow (ArrowChoice(right))

data Tree a = 
    Empty | Node a (Tree a) (Tree a)
    deriving Show

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

treeSum :: (Num n) => Tree n -> n
treeSum Empty = 0
treeSum (Node val left right) = val + treeSum left + treeSum right

foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree _ nullValue Empty = nullValue
foldTree op nullValue (Node val left right) = op val $ op (foldTree op nullValue right) (foldTree op nullValue left) 

maxSumPath :: (Ord n, Num n) => Tree n -> n
maxSumPath Empty = 0
maxSumPath (Node val left right) = val + max (maxSumPath left) (maxSumPath right)

myIsLeaf :: Tree a -> Bool
myIsLeaf Empty = False
myIsLeaf (Node _ Empty Empty) = True
myIsLeaf _ = False

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False 

prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node val left right)
    | isEmpty left && isEmpty right = Empty
    | otherwise                     = Node val (prune left) (prune right)

bloom :: Tree a -> Tree a
bloom Empty = Empty
bloom (Node val left right) 
    | isEmpty left && isEmpty right = Node val (Node val Empty Empty) (Node val Empty Empty)
    | otherwise                     = Node val (bloom left) (bloom right)

invertTree :: Tree a -> Tree a
invertTree Empty = Empty
invertTree (Node val left right) = Node val (invertTree right) (invertTree left)

level :: Tree a -> Int -> [a]
level Empty _ = []
level _ (-1)    = []
level (Node val left right) 0 = [val]
level (Node val left right) n = level left (n - 1) ++ level right (n - 1)

t :: Tree Integer
t = Node 5 (Node 10 Empty Empty) (Node 3 Empty (Node 20 Empty Empty))

main :: IO()
main = do
    let res = level t 3
    putStrLn ("Answers is: " ++ show res)