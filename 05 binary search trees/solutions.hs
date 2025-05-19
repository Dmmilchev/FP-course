import Data.IntMap (insert, empty)
import Control.Arrow (ArrowChoice(right))
data BST a = 
    Empty | Node a (BST a) (BST a)
    deriving Show

insertBST :: (Ord a) => BST a -> a -> BST a
insertBST Empty elem = Node elem Empty Empty
insertBST (Node val left right) elem
    | val > elem = Node val (insertBST left elem) right
    | otherwise  = Node val left (insertBST right elem)

insertList :: (Ord a) => BST a -> [a] -> BST a
insertList = foldl insertBST

traverseBST :: (Ord a) => BST a -> [a]
traverseBST Empty = []
traverseBST (Node val left right) = traverseBST left ++ [val] ++ traverseBST right

bstSort :: (Ord a) => [a] -> [a]
bstSort l = traverseBST $ insertList Empty l

t :: BST Int
t = insertList Empty [3, 4, 6, -1, 2, 10, -5]

main :: IO()
main = do
    let res = traverseBST t
    putStrLn ("Answer is: " ++ show res)

-- The following problems are pretty much identical to the above. 
-- I don't see any benefit of solving them. 