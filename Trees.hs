-- a is Int for example
data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

addToTree :: (Ord a) => a -> Tree a -> Tree a --add element to existing tree
addToTree n Empty = Node Empty n Empty
addToTree n (Node left value right) 
  | n<value = Node (addToTree n left) value right
  | otherwise = Node left value (addToTree n right)

height :: Tree a -> Int
height Empty = 0
height (Node Empty value Empty) = 1
height (Node left value right)
  | height left > height right = 1 + height left 
  | otherwise = 1 + height right


isBalanced :: (Ord a) => Tree a -> Bool
isBalanced Empty = True 
isBalanced (Node left value right) = height left == height right


addMultipleToTree :: (Ord a) => [a] -> Tree a -> Tree a --insert multiple elements to tree
addMultipleToTree [] tree = tree
addMultipleToTree (x:xs) tree = addMultipleToTree xs (addToTree x tree)


buildTree :: (Ord a) => [a] -> Tree a --build tree from a list of elements
buildTree xs = addMultipleToTree xs Empty


--show behavour
main :: IO ()
main =  do

let x_list = ["5","3", "4", "6", "7"]
let x = buildTree x_list

putStrLn("Creating a tree from: ")
print(x_list)
putStrLn("The tree:")
print(x)
putStrLn("Its height:")
print(height x)

putStrLn("Is balanced:")
print(isBalanced x)

putStrLn("Creating lopsided tree from: ")
let y_list = ["1","2", "3", "4"]
print(y_list)
let y = buildTree y_list
putStrLn("The tree:")
print(y)

putStrLn("Its height:")
print(height y)

putStrLn("Is balanced:")
print(isBalanced y)