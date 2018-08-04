import Data.List
-------------------------------------------
lastButOne a = if null (tail (tail a))
			   then head a
               else lastButOne (tail a)
			   

-------------------------------------------			
   
lastOne a =  if null (tail a)
			 then head a
			 else lastOne (tail a)		
			 
-------------------------------------------			
 
sumList [] = 0
sumList a = head a + sum (tail a)

-------------------------------------------

productList [] = 1
productList a = head a * productList (tail a)

-- main = print (lastOne [1,4,51,2,3,4,5,6])
--------------------------------------------
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList(xs)

---------------------------------------------
len :: [a] -> Int
len a = if null a then 0 else 1 + len (tail a)

---------------------------------------------

mean :: [Double] -> Double
-- mean l =snd(inner l) / fst(inner l)
-- where inner [] = (0,0)
-- inner (x:xs) = (fst(inner xs) + 1, snd(inner xs) + x)

---------------------------------------------
mean xs | null xs = 0.0| otherwise = (sum xs) / (count xs) 
    where sum [] = 0 
          sum (x:xs) = x + sum xs 
          count [] = 0 
          count (_:xs) = 1 + count xs
      
----------------------------------------------
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]
-----------------------------------------------
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]
-----------------------------------------------
-- isPalindrome :: [a] -> Bool
{-isPalindrome (x:xs) = if x == lastOne xs
                         then isPalindrome xs
          -}               
                         
isPalindrome x| x == [] = True
              | null (tail x) = True
              | otherwise = (a == b) && (isPalindrome (tail (init x)))
    where a = head x
          b = last x

----------------------------------------------
sortByLen [x] = [x]
sortByLen (x:xs) = if (length x) <= (length (head xs))
                      then x : (sortByLen xs)
                      else (head xs) :(sortByLen (x:tail xs))
sortByLen [] = []
----------------------------------------------
srtBy :: Ord a => (t -> a) -> [t] -> [t]
srtBy f [] = []
srtBy f (x:xs) = srtBy' f [x] xs
    where srtBy' f sorted [] = sorted
          srtBy' f sorted (x:xs) = srtBy' f (insertIntoSorted f x sorted) xs 
            where insertIntoSorted f x [] = [x] 
                  insertIntoSorted f x (y:ys)
                      | f x <= f y = x:y:ys
                      | otherwise = y:insertIntoSorted f x ys

srtByLen a = srtBy len a
-------------------------------------------------
-------Not Working ------------------------------
-- sortLength :: [[a]] -> [[a]]
-- sortLength [] = []
-- sortLength [x] = [x]
-- sortLength (x:xs) = sortLength ltx ++ [x] ++ sortLength gtx
--     where (ltx,gtx) = partition f xs
--                 where f l| length l <= length x = True
--                       | otherwise = False
--------------------------------------------------
{-
lensort' [] = []
lensort' [x] = [x]
lensort' (x:xs) = let ys = lensort' xs
                      y = head ys
                  in if length x >= length y
                        then y:lensort' (x:tail ys)
                        else x:ys-}
-------------------------------------------------
lensort' [] = []
lensort' [x] = [x]
lensort' (x:xs) = if len x >= len (head (lensort' xs))
                     then head (lensort' xs):lensort' (x:tail (lensort' xs))
                        else x:(lensort' xs)
-------------------------------------------------
data List a = Cons a (List a)
            | Nil
              deriving (Show)
			  
			  
toList :: (List a) -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

---------------------------------------------------
intersperse' a [] = []
--intersperse' a b = [a]++b
intersperse' a list = [head list] ++[a] ++ intersperse a (tail list)

main = print ( (intersperse' "," ["ads","tdgdf","dSGSDG","wrewfdg"]) )

---------------------------------------------------
