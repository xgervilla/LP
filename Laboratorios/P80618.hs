data Queue a = Queue [a] [a]
     deriving (Show)

create::Queue a
create = Queue [][]

push :: a-> Queue a -> Queue a
push x (Queue l r) = Queue l (x:r)

pop :: Queue a -> Queue a
pop (Queue [][]) = (Queue [][])
pop (Queue [] x) = Queue (tail (reverse x)) []
pop (Queue x y) = Queue (tail x) y

top:: Queue a -> a
top (Queue [] y) = head (reverse y)
top (Queue x y) = head x

empty :: Queue a -> Bool
empty (Queue [][]) = True
empty _ = False

instance Eq a => Eq (Queue a)
     where (Queue x1 y1) == (Queue x2 y2) = x1 ++ (reverse y1) == x2 ++ (reverse y2)


translation :: Numb b => b -> Queue b -> Queue b
translation op (Queue l r) = (Queue [] [])

kfilter :: (p-> Bool) -> Queue p -> Queue p
kfilter op (Queue l r) = (Queue [] [])