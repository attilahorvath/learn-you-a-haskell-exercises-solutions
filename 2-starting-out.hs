{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate :: [a] -> a
penultimate l = last (init l)

-- Solution without using built-in functions
penultimate' :: [a] -> a
penultimate' [] = error "empty list"
penultimate' [_] = error "list with only one element"
penultimate' [x, _] = x
penultimate' (_:xs) = penultimate' xs

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK :: Int -> [a] -> a
findK k l = l !! k

-- Solution without using built-in functions
findK' :: Int -> [a] -> a
findK' _ [] = error "out of bounds"
findK' 0 (x:_) = x
findK' k (_:xs) = findK' (k - 1) xs

-- Determine if list l is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate :: [a] -> [a]
duplicate xs = concat [[x, x] | x <- xs]

-- Solution without using built-in functions
duplicate' :: [a] -> [a]
duplicate' [] = []
duplicate' (x:xs) = x : x : duplicate' xs

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike :: [a] -> [b] -> [(a, b)]
ziplike _ [] = []
ziplike [] _ = []
ziplike (x:xs) (y:ys) = (x, y) : ziplike xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex :: Int -> [a] -> ([a], [a])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK :: Int -> [a] -> [a]
dropK k l = take k l ++ drop (k + 1) l

-- Solution without using built-in functions
dropK' :: Int -> [a] -> [a]
dropK' _ [] = []
dropK' 0 (x:xs) = xs
dropK' k (x:xs) = x : dropK' (k - 1) xs

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice :: Int -> Int -> [a] -> [a]
slice i k l = take (k - i) (drop i l)

-- Solution without using built-in functions
slice' :: Int -> Int -> [a] -> [a]
slice' _ _ [] = []
slice' _ 0 _ = []
slice' 0 k (x:xs) = x : slice' 0 (k - 1) xs
slice' i k (_:xs) = slice' (i - 1) (k - 1) xs

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: a -> Int -> [a] -> [a]
insertElem x k l = take k l ++ [x] ++ drop k l

-- Solution without using built-in functions
insertElem' :: a -> Int -> [a] -> [a]
insertElem' x 0 xs = x:xs
insertElem' x k [] = error "out of bounds"
insertElem' e k (x:xs) = x : insertElem' e (k - 1) xs

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate n l = drop n l ++ take n l
