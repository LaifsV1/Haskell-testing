-- author Yu-Yang Lin

import Data.Ratio

-- ###############################
-- =====  LIST EXPERIMENTS  ======
-- ###############################

length_ l = foldl (+) 0 (map (\x -> 1) l)

-- Sorted Functions
sorted,sorted1,sorted2, sorted3, sorted4, sorted5 :: Ord a => [a] -> Bool
sorted []     = True                                                                    -- standard recursion
sorted (x:[]) = True
sorted (x:xs) = x <= (head xs) && sorted xs
sorted1 xs    = foldr (&&) True (map (\(a,b) -> a<=b) (zip xs (tail xs)))               -- foldr by mapping and zipping
sorted2 []    = True                                                                    -- foldl using tuples
sorted2 xs    = (\(x,y) -> y) (foldl (\(x,y) a -> (a,x<=a && y)) (head xs, True) xs)
                                                                                        -- foldr by mapping and foldr zipping
sorted3 xs    = foldr (&&) True (map (\(a,b) -> a<=b) ((foldr (\ a f bs -> if null bs then [] else (a,(head bs)) : (f (tail bs))) (const [])) xs (tail xs)))
sorted4 []    = True                                                                    -- foldr returning a function
sorted4 xs    = foldr (\a f b -> (a >= b) && f a) (const True) (tail xs) (head xs)
sorted5 [] = True                                                                       -- tail recursion
sorted5 xs = sorted5' xs (head xs) True
sorted5' []     a b = b
sorted5' (x:xs) a b = sorted5' xs x (a<=x && b)
-- *Main> sorted [1..1000000]
-- True
-- (0.98 secs, 141708068 bytes)
-- *Main> sorted1 [1..1000000]
-- True
-- (0.28 secs, 169984968 bytes)
-- *Main> sorted2 [1..1000000]
-- True
-- (1.11 secs, 195189148 bytes)
-- *Main> sorted3 [1..1000000]
-- True
-- (1.58 secs, 301292440 bytes)
-- *Main> sorted4 [1..1000000]
-- True
-- (0.66 secs, 133738604 bytes)
-- *Main> sorted5 [1..1000000]
-- True
-- (0.98 secs, 142016612 bytes)

myzip, myzip2 :: [a] -> [b] -> [(a,b)]
myzip = (foldr (\ a f bs -> if null bs then [] else (a,(head bs)):(f (tail bs))) (const []))
myzip2 = foldr step (const [])
    where step a f (b:bs) = (a,b):(f bs)
          step a f [] = []
          
          
-- ###############################
-- =====  TREE EXPERIMENTS  ======
-- ###############################

data BT a = Empty | Node (BT a) a (BT a) deriving Show

foldt :: (a -> b -> b -> b) -> b -> BT a -> b
foldt f z Empty = z
foldt f z (Node l x r) = f x (foldt f z l) (foldt f z r)

foldt' :: (b -> b -> a -> b) -> b -> BT a -> b
foldt' f z Empty = z
foldt' f z (Node l x r) = f (foldt' f z l) (foldt' f z r) x

lmost :: BT a -> a
lmost Empty             = error "Empty Tree"
lmost (Node Empty x r)  = x
lmost (Node l x r)      = lmost l

rmost :: BT a -> a
rmost Empty             = error "Empty Tree"
rmost (Node l x Empty)  = x
rmost (Node l x r)      = rmost r

const2 :: a -> b -> c -> a
const2 x _ _ = x

-- Insert BST
insertbt :: Ord a => a -> BT a -> BT a
insertbt a Empty        = Node Empty a Empty
insertbt a (Node l x r) = if a<=x then Node (insertbt a l) x r else Node l x (insertbt a r)

-- List to BST
list2bst1, list2bst2, list2bst3 :: Ord a => [a] -> BT a
list2bst1 [] = Empty                            -- using foldr
list2bst1 xs = foldr insertbt Empty xs

list2bst2 [] = Empty                            -- using standard recursion
list2bst2 (x:xs) = insertbt x (list2bst2 xs)

list2bst3 xs = list2bst3' xs Empty              -- using tail recursion
list2bst3' []     t = t
list2bst3' (x:xs) t = list2bst3' xs (insertbt x t)
--all seem to be equal in complexity.

-- BST Check Functions
isbst1, isbst2, isbst3:: Ord a => BT a -> Bool
isbst1 Empty = True                             -- using foldt by returning a function
isbst1 t = foldt (\x l r a b -> a <= x && x <= b && (l a x) && (r x b)) (const2 True) t (lmost t) (rmost t)

isbst2 t = isbst2' t (lmost t) (rmost t)        -- using standard recursion
isbst2' Empty        lo hi = True
isbst2' (Node l x r) lo hi = lo <= x && x <= hi && (isbst2' l lo x) && (isbst2' r x hi)

isbst3 t = isbst3' t (lmost t) (rmost t) True   -- using tail recursion
isbst3' Empty        lo hi b = b
isbst3' (Node l x r) lo hi b = (isbst3' l lo x b') && (isbst3' r x hi b') where b' = lo <= x && x <= hi
                                                -- using foldt to flatten the tree and and check if its sorted
isbst4 t = sorted4 (foldt (\x l r -> l ++ [x] ++ r) [] t)

isbst5 Empty = True
isbst5 t = (\(x,y) -> y) (isbst5' t)
isbst5' (Node l x r) = foldt (\x (lo,s1) (hi,s2) -> (x,lo <= x && x <= hi && s1 && s2)) (x,True) (Node l x r)

--all take the same amount of time and use similar amounts of space. isbst4 uses about 8% more space.

-- ###############################
-- == RECURSIVE SEQUENCES TESTS ==
-- ###############################

-- ==========================
-- Fibonacci and Stern-Brocot
-- ==========================

-- Standard squared complexity Fibonacci
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Linear complexity Fibonacci      - map fib2 [0..700]     ~ (0.83 secs, 67859720 bytes)
fib2 :: Integer -> Integer
fib2 n = fib2aux (n,1,1)

fib2aux :: (Integer, Integer, Integer) -> Integer
fib2aux (0,a,b) = a
fib2aux (1,a,b) = b
fib2aux (n,a,b) = fib2aux (n-1,b,a+b)

-- Linear Fibonacci list builder    - fibl 700              ~ (0.59 secs, 24899352 bytes)
fibl :: Integer -> [Integer]
fibl n = fiblaux (n,1,1)

fiblaux :: (Integer, Integer, Integer) -> [Integer]
fiblaux (0,a,b) = [a]
fiblaux (n,a,b) = a:fiblaux (n-1,b,a+b)

-- Stern-Brocot Sequence list builder
-- NOTE: Not sure about its complexity, it has linear iterations, but appending could be inefficient.
-- Maybe we have, once again, some lazy eval magic going on there, which makes append efficient.
-- On the other hand, however, the stack depth could get huge for this.
sbseq :: Integer -> [Integer]
sbseq n = take (fromIntegral n) (1:1:sbaux (n,[1,1]))

sbaux (0,ls) = (head ls)+(head (tail ls)):[head (tail ls)]
sbaux (n,ls) = (head ls)+(head (tail ls)):(head (tail ls)):sbaux (n-1,(tail ls)++((head ls)+(head (tail ls)):[head (tail ls)]))

-- Stern-Brocot tree builder
sbtree :: Integer -> BT Rational
sbtree n = sbtaux n 1 1

sbtaux 0 a b = Empty
sbtaux n a b = Node (sbtaux (n-1) a (a+b)) (a%b) (sbtaux (n-1) (a+b) b)

-- Stern-Brocot sequence list builder using trees
sbseq2 :: Integer -> [Integer]
sbseq2 n = take (fromIntegral n) (bfqueue [sbtree n])
    where bfqueue [] = []
          bfqueue (Empty : xs) = bfqueue xs
          bfqueue (Node l x r : xs) = (numerator x) : bfqueue (xs ++ [l,r])

-- Stern-Brocot rational numbers list builder
-- NOTE: Similar to sbseq, it seems to run fine. However, it could actually be quite inefficient.
-- It's a conjecture rooting from sbseq n generating more than n elements.
-- Once again, however, lazy eval could be making this more efficient.
sbrat,sbrat2,sbrat3 :: Integer -> [Rational]
sbrat n = take (fromIntegral n) (sbrataux (sbseq (n+1)))

sbrataux []       = []
sbrataux [x]      = []
sbrataux (x:y:xs) = (x % y):(sbrataux (y:xs))

-- Stern-Brocot rational numbers list builder by breadth-first (levelorder) flattening a Stern-Brocot tree
-- NOTE: Although all of these seem to run with similar complexity, sbrat2 seems to be the slowest.
sbrat2 n = take (fromIntegral n) (bfqueue [sbtree n])
    where bfqueue [] = []
          bfqueue (Empty : xs) = bfqueue xs
          bfqueue (Node l x r : xs) = x : bfqueue (xs ++ [l,r])

-- Stern-Brocot rational numbers list builder using folds and zips
sbrat3 n = foldr (\(a,b) y -> a%b : y) [] (take (fromIntegral n) (zip a (tail a))) where a = sbseq (n+1)

-- ======================
-- Dragon Curve functions
-- ======================

-- bit list to LR list
toLR [] = []
toLR (x:xs) = if x==0 then 'L':(toLR xs) else 'R':(toLR xs)

-- nth turn, this is limited by the size of Int
nthTurn n = last (dragon n)

-- nth turn, works for any input. This, however, is not too efficient for large numbers.
-- interestingly, the functions works out the turn instantly for any number 2^x.
nthTurn2 n = (dragonaux [] 0)!!n

-- dragon curve by appending [1] to the end, to then append the previous iteration with the middle bit flipped
-- words if we are starting with a 1 by convention (if 1 is the first turn).
-- this is similar to unfolding the dragon, but as an infinite list.
-- this solution seems to be the fastest of the ones implemented.
-- I haven't calculated it's complexity, however, the function has to go through (n/2) items each run, and append n+1 items
-- after the current n elements. Since the number of elements roughly doubles each iteration (2n+1), after k iterations, I
-- would expect the number of elements in the list to be T(k)=(2^k-1)+T(k-1) where T(0)=1. From this, we go through 1/2 of
-- all elements, or roughly, T(k-1) items.
dragon, dragon2 :: Int -> [Integer]
infiniteDragon :: [Integer]

dragon n = take n (dragonaux [] 0)
infiniteDragon = dragonaux [] 0

dragonaux xs 0 = [1] ++ (dragonaux [1] 1)
dragonaux xs n = 1 : ys ++ (dragonaux (xs ++ [1] ++ ys) (n*2))
    where mid_comp (x:xs) 1 = if x==0 then 1:xs else 0:xs
          mid_comp (x:xs) n = x:(mid_comp xs (n-1))
          ys = mid_comp xs n

-- dragon curve by unfolding the dragon iteratively. This is done by replacing every segment with alternating turns.
-- this can be simplified by creating a string of alternating 0s and 1s, and then adding the previous elements between
-- the string elements.
-- eg. (110) -> 1 (1) 0 (1) 1 (0) 0 -> (1101100)
-- this solution seems to be much slower than the previous one, requiring more space too.
dragon2 n = take n (dragon2' [1] n)

dragon2' xs 0 = xs
dragon2' xs n = dragon2' (dragon2aux xs 1) (n-1)

dragon2aux [] n = [n]
dragon2aux (x:xs) 0 = 0:x:(dragon2aux xs 1)
dragon2aux (x:xs) 1 = 1:x:(dragon2aux xs 0)

-- ####################
-- == CALCULATING PI ==
-- ####################

-- From Unbounded Spigot Algorithms for the Digits of Pi by Jeremy Gibbons
-- by testing, I've noticed that the function slows down very quickly.
--take 1 spigot     (0.00 secs, 525624 bytes)
--take 10 spigot    (0.00 secs, 521300 bytes)
--take 100 spigot   (0.00 secs, 2051308 bytes)
--take 1000 spigot  (0.11 secs, 132889928 bytes)
--take 10000 spigot (9.20 secs, 16571941788 bytes)
spigot :: [Integer]
spigot = g(1,0,1,1,3,3) where
    g (q,r,t,k,n,l) = 
        if 4*q+r-t < n*t
        then n : g (10*q, 10*(r-n*t), t, k, div (10*(3*q+r)) t - 10*n, l)
        else g (q*k, (2*q+r)*l, t*l, k+1, div (q*(7*k+2)+r*l) (t*l), l+2)

pi_digits :: [Char]
pi_digits = insertPoint digits'
    where insertPoint (x:xs) = x:'.':xs
          digits' = map (head . show) spigot
