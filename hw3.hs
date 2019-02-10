--Created by Min Hee Son (11/29/18): ECS140A Homework 3

--removes duplicate elements from a given list and returns the resulting list
myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates list 
   | null list = list  
   | elem (head list) (tail list) = myremoveduplicates (tail list)  
   | otherwise = (head list):(myremoveduplicates (tail list))

--removes duplicate elements from a given list and returns the resulting list
myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:[]) = [x]
myremoveduplicates_pm (x:y:[]) = if x == y then [x] else x:y:[]
myremoveduplicates_pm (x:xs) 
   | elem x xs = myremoveduplicates_pm xs 
   | otherwise = x:(myremoveduplicates_pm xs)

--finds common elements from two lists and returns them in a new list
myintersection :: (Foldable t, Eq a) => [a] -> t a -> [a]
myintersection list1 list2 
   | null list1  || null list2 = [] 
   | elem (head list1) list2 = (head list1):myintersection (tail list1) list2 
   | otherwise = myintersection (tail list1) list2 

--finds common elements from two lists and returns them in a new list
myintersection_pm :: (Foldable t, Eq a) => [a] -> t a -> [a]
myintersection_pm [] [] = []
myintersection_pm list1 [] = []
myintersection_pm [] list2  = []
myintersection_pm (x:xs) y 
   | elem x y = x:(myintersection_pm xs y) 
   | otherwise = myintersection_pm xs y


--returns the last element from a list
mylast :: Eq a => [a] -> [a]
mylast list
   | null list || (tail list) == [] = list 
   | otherwise = mylast (tail list)

--returns the last element from a list
mylast_pm :: Eq a => [a] -> [a]
mylast_pm [] = []
mylast_pm (x:[]) = [x]
mylast_pm (x:xs) = mylast xs

--returns a list with its elements reversed
myreverse :: Eq a => [a] -> [a]
rev :: [a] -> [a] -> t
myreverse list 
   | null list || (tail list) == [] = list 
   | otherwise = rev list []
   where
    rev [] reversed = reversed
    rev list reversed = rev (tail list) (head list:reversed)

--returns a list with its elements reversed
myreverse_pm :: [a] -> t
rev_p :: [a] -> [a] -> t
myreverse_pm [] = []
myreverse_pm list = rev_p list []
   where
    rev_p [] l = l
    rev_p (x:xs) l = rev_p xs (x:l)

--substitutes 'x' for every element 'y' and returns the new list 
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall x y list 
   | null list = []
   | list == y:[] = [x]
   | y == (head list) = x:(myreplaceall x y (tail list))
   | otherwise = (head list):(myreplaceall x y (tail list))

--substitutes 'x' for every element 'y' and returns the new list 
myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm x y [] = []
myreplaceall_pm x y (z:[]) = if z == y then [x] else [z]
myreplaceall_pm x y (z:zs) 
   | y == z = x:(myreplaceall_pm x y (zs))
   | otherwise = z:(myreplaceall_pm x y (zs))

