data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size:: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1+ size left + size right

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1+max (height left) (height right)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node a1 left1 right1) (Node a2 left2 right2) = a1==a2 && (equal left1 left2) && (equal right1 right2)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node a1 left1 right1) (Node a2 left2 right2) = a1==a2 && ((isomorphic left1 left2) && (isomorphic right1 right2) || (isomorphic left1 right2) && (isomorphic left2 right1))

--raiz, izquierdo, derecho
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a1 left right) = [a1] ++ (preOrder left) ++ (preOrder right)

--izquierdo, raiz, derecho
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a1 left right) = (inOrder left) ++[a1]++ (inOrder right)

--izquierdo, derecho, raiz
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a1 left right) = (postOrder left) ++ (postOrder right) ++ [a1]


breadthFirst :: Tree a -> [a]
breadthFirst x = bfs [x]

bfs :: [Tree a] -> [a]
bfs [] = []
bfs (Empty:ts) = bfs ts
bfs ((Node x left right):ts) = x:(bfs(ts++[left,right]))


build [] [] = Empty
build (x:xs) ys = (Node x t1 t2)
   where t1 = build x1s y1s
         t2 = build x2s y2s
         (y1s, y2s) = split x ys
         (x1s, x2s) = splitAt (length y1s) xs

split x xs = (p1, tail p2)
   where (p1,p2) = splitAt (indexOf xs) xs
         indexOf (y:ys)
             | x == y = 0
             | otherwise = 1+indexOf ys


overlap :: (a->a->a) -> Tree a -> Tree a -> Tree a
overlap func Empty Empty = Empty
overlap func (Node a1 left right) Empty = (Node a1 left right)
overlap func Empty (Node a1 left right) = (Node a1 left right)
overlap func (Node a1 left1 right1) (Node a2 left2 right2) = (Node (func a1 a2) (overlap func left1 left2) (overlap func right1 right2))