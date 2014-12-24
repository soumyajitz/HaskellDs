import Data.List
import Test.QuickCheck
--import qualified Data.Map as M
--import Data.Hashable
--import Data.HashSet

main=putStrLn "Project Start"

f::[String]->[Int]
f=map read

f1:: String->Int
f1=read 

head1       :: [a] -> a
head1 (x:_) = x
head1 []    = error "Prelude.head: empty list"

minimum1    :: (Ord a) => [a] -> a
minimum1 [] =  error "Prelude.minimum: empty list"
minimum1 xs =  foldl1 min xs

maximum1    :: (Ord a) => [a] -> a
maximum1 [] =  error "Prelude.minimum: empty list"
maximum1 xs =  foldl1 max xs



--STACK and QUEUE
convertStringInt::String -> Int
convertStringInt = read

stack:: [Int] -> IO ()
stack [] = do
          print "list is empty. good bye"
stack (y:ys) = do
            print "current stack"
            print (y:ys)
            print "If you want to quit enter quit. Or enter push or pop"
            x <- getLine
            if x == "push"
              then do
                    print "enter number you want to push"
                    z <- getLine
                    let temp = convertStringInt $ z
                    stack $ (temp: (y:ys))
              else if x == "pop"
                    then do
                          print "pop element is: "
                          print y
                          stack ys
                    else do
                          print "good bye"

input_stack:: Int -> IO ()
input_stack x = do
          temp <- getArrayFromUser x []
          stack temp

getArrayFromUser:: Int -> [Int] -> IO [Int]
getArrayFromUser x []= do
                    print "enter number you want to push"
                    z <- getLine
                    let temp = convertStringInt $ z
                    getArrayFromUser x [temp]

getArrayFromUser x (y:ys)= do
            if x == length (y:ys)
              then do
                    return (y:ys)
              else do
                    print "enter number you want to push"
                    z <- getLine
                    let temp = convertStringInt $ z
                    getArrayFromUser (x) (temp:(y:ys))


--QUEUE
queue:: [Int] -> IO ()
queue [] = do
          print "list is empty. good bye"
queue (y:ys) = do
            print "current queue"
            print (y:ys)
            print "If you want to quit enter quit. Or enter insert or remove"
            x <- getLine
            if x == "insert"
              then do
                    print "enter number you want to insert"
                    z <- getLine
                    let temp = convertStringInt $ z
                    queue $ (temp: (y:ys))
              else if x == "remove"
                    then do
                          print "removed element is: "
			  let temp1 = ys 
                          print $ last temp1
			  
                          queue (init (y:ys))
                    else do
                          print "good bye"

input_queue:: Int -> IO ()
input_queue x = do
          temp <- getArrayFromUserq x []
          queue temp

getArrayFromUserq:: Int -> [Int] -> IO [Int]
getArrayFromUserq x []= do
                    print "enter number you want to insert"
                    z <- getLine
                    let temp = convertStringInt $ z
                    getArrayFromUserq x [temp]

getArrayFromUserq x (y:ys)= do
            if x == length (y:ys)
              then do
                    return (y:ys)
              else do
                    print "enter number you want to insert"
                    z <- getLine
                    let temp = convertStringInt $ z
                    getArrayFromUserq (x) (temp:(y:ys))
--END
					
--LINEAR and BINARY SEARCH DS

--LINEAR SEARCH
search ::Eq a=> [a] -> a -> Bool 
search [] _ =error"Not found"
search (x:xs) y = x==y || search xs y

linear_search=do
			putStrLn "Enter the List of 6 Numbers"
			str<-sequence[getLine,getLine,getLine,getLine,getLine,getLine]
			let in1=f str
			putStrLn "Enter the number you want to search"
			x<-getLine
			let y1=f1 x
			let in2=search in1 y1
			print in2

--BINARY SEARCH
bins :: [Int] -> Int -> Int -> Int -> Int -- list, value, low, high, return int
bins xs value low high
   | high < low       = -1
   | xs!!mid > value  = bins xs value low (mid-1)
   | xs!!mid < value  = bins xs value (mid+1) high
   | otherwise        = mid
   where
   mid = (low + high) `div` 2
 
binary_search=do
			putStrLn "Enter the List of 6 Numbers"
			str<-sequence[getLine,getLine,getLine,getLine,getLine,getLine]
			let in1=f str
			putStrLn "Enter the number you want to search"
			x<-getLine
			let x1=f1 x
			putStrLn "Enter low"
			y<-getLine
			let y1=f1 y
			putStrLn "Enter high"
			z<-getLine
			let z1=f1 z
			let in2=bins in1 x1  y1  z1
			print in2  
--END
			
--SORTING DATA-STRUCTURES
--MERGE SORT
msort_merge :: (Ord a) => [a] -> [a] -> [a]
msort_merge [] xs = xs
msort_merge xs [] = xs
msort_merge (x:xs) (y:ys)
    | (x < y) = x:msort_merge xs (y:ys)
    | otherwise = y:msort_merge (x:xs) ys
 
msort_divide :: [a] -> ([a], [a])
msort_divide xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
merge_Sort :: (Ord a) => [a] -> [a]
merge_Sort xs 
    | (length xs) > 1 = msort_merge (merge_Sort ls) (merge_Sort rs)
    | otherwise = xs
    where (ls, rs) = msort_divide xs
	
	
merge_sort=do
		putStrLn "Enter 4 numbers"
		str<-sequence[getLine,getLine,getLine,getLine]
		let in1=f str
		let in2=merge_Sort in1
		print "The Sorted List is"
		print in2
--Applying QuickCheck to test properties
checkm_apply2 xs = merge_Sort (merge_Sort xs) == merge_Sort xs
checkm_minimum xs         =  not (null xs) ==> head1 (merge_Sort xs) == minimum1 xs
checkm_maximum xs         =  not (null xs) ==>last (merge_Sort xs) == maximum1 xs
checkm_append xs ys       = not (null xs) ==>
                           not (null ys) ==>
                                             head1 (merge_Sort (xs ++ ys)) == min (minimum1 xs) (minimum1 ys)	
--QUICK SORT
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let lesserthanpivot = quicksort [a | a <- xs, a <= x]  
        greaterthanpivot = quicksort [a | a <- xs, a > x]  
    in  lesserthanpivot ++ [x] ++ greaterthanpivot  

quick_sort=do
		putStrLn "Enter 5 Numbers"
		str<-sequence[getLine,getLine,getLine,getLine,getLine]
		let in1=f str
		let in2=quicksort in1
		print "The Sorted List is"
		print in2	
--Applying QuickCheck to test properties
checkq_apply2 xs = quicksort (quicksort xs) == quicksort xs
checkq_minimum xs         =  not (null xs) ==> head1 (quicksort xs) == minimum1 xs
checkq_maximum xs         =  not (null xs) ==>last (quicksort xs) == maximum1 xs
checkq_append xs ys       = not (null xs) ==>
                           not (null ys) ==>
                                             head1 (quicksort (xs ++ ys)) == min (minimum1 xs) (minimum1 ys)
--SELECTION SORT

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = leastvalue : (selectionSort remaininglist)
                   where leastvalue = minimum xs
			 remaininglist = delete leastvalue xs
			 
selection_sort=do
		putStrLn "Enter 5 Numbers"
		str<-sequence[getLine,getLine,getLine,getLine,getLine]
		let in1=f str
		let in2=selectionSort in1
		print "The Sorted List is"
		print in2

--Applying QuickCheck to test properties
checks_apply2 xs = selectionSort (selectionSort xs) == selectionSort xs
checks_minimum xs         =  not (null xs) ==> head1 (selectionSort xs) == minimum1 xs
checks_maximum xs         =  not (null xs) ==>last (selectionSort xs) == maximum1 xs
checks_append xs ys       = not (null xs) ==>
                           not (null ys) ==>
                                             head1 (selectionSort (xs ++ ys)) == min (minimum1 xs) (minimum1 ys)
		
--INSERTION SORT

insertionSort :: (Ord a) => [a] -> [a]
insertionSort xs = iSort xs []
          where
		     iSort :: (Ord a) => [a] -> [a]-> [a]
		     iSort [] sorted = sorted
		     iSort (x:xs) sorted = iSort xs n_Sorted
		       where
		         n_Sorted = [y | y <- sorted, y <= x] ++ 
                                     [x] ++ [y | y <- sorted, y>x]

insertion_sort=do
		putStrLn "Enter 5 Numbers"
		str<-sequence[getLine,getLine,getLine,getLine,getLine]
		let in1=f str
		let in2=insertionSort in1
		print "The Sorted List is"
		print in2

--Applying QuickCheck to test properties
checki_apply2 xs = insertionSort (insertionSort xs) == insertionSort xs
checki_minimum xs         =  not (null xs) ==> head1 (insertionSort xs) == minimum1 xs
checki_maximum xs         =  not (null xs) ==>last (insertionSort xs) == maximum1 xs
checki_append xs ys       = not (null xs) ==>
                           not (null ys) ==>
                                             head1 (insertionSort (xs ++ ys)) == min (minimum1 xs) (minimum1 ys)

--BUBBLE SORT

interchange :: Ord a => [a] -> [a]
interchange [] = []
interchange [x] = [x]
interchange (x1:x2:xs)
        | x1 > x2 = x2 : interchange(x1:xs)
        | otherwise = x1 : interchange(x2:xs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs
        | interchange xs == xs = xs       
        | otherwise = bubbleSort $ interchange xs

bubble_sort=do
        putStrLn "Enter 4 numbers"
        strk<-sequence[getLine,getLine,getLine,getLine]
        let ink1=f strk
        let ink2=bubbleSort ink1
        print "The Sorted List is"
        print ink2


--Applying QuickCheck to test properties
checkb_apply2 xs = bubbleSort (bubbleSort xs) == bubbleSort xs
checkb_minimum xs         =  not (null xs) ==> head1 (bubbleSort xs) == minimum1 xs
checkb_maximum xs         =  not (null xs) ==>last (bubbleSort xs) == maximum1 xs
checkb_append xs ys       = not (null xs) ==>
                           not (null ys) ==>
                                             head1 (bubbleSort (xs ++ ys)) == min (minimum1 xs) (minimum1 ys)


--Tree DS
data Tree a= EmptyTree|Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singletree:: a-> Tree a
singletree x=Node x EmptyTree EmptyTree

treeValues::(Ord a)=>a->Tree a->Tree a
treeValues x EmptyTree=singletree x
treeValues x (Node a left right)
  |x==a=Node x right left
  |x<a= Node a(treeValues x left) right
  |x>a= Node a left (treeValues x right)

elemFound::(Ord a)=>a->Tree a->Bool
elemFound x EmptyTree=False
elemFound x (Node a  left right)
  |x==a=True
  |x<a=elemFound x left
  |x>a=elemFound x right

binary_search_tree=do
          putStrLn "Enter the List of Numbers to be inserted in the tree:"
          strtree<-sequence[getLine,getLine,getLine,getLine,getLine,getLine,getLine]
          let numsTree=foldr treeValues EmptyTree strtree
          print numsTree
          putStrLn "Do you want to search the tree-yes(y)/no(n)"
          decision<-getLine
          if decision=="y"
            then do
                putStrLn "Enter a number to search within the tree:"
                k<-getLine
                let search1=k `elemFound` numsTree
                if search1 == True
                then do
                print "The Element is Found"
                else do
                print "The Element is not Found"
          else
            print "Good Bye-Thank You"
--HASH TABLE()
{--
myMap :: M.Map  Integer String
myMap = M.fromList $ zip [1..10] ["a1","a2","a3","a4","a5","a6","a7","a8","a9","a10"] 

insertedMap1 :: M.Map String Integer
insertedMap1 = M.insert 17 "hello1"  myMap 


insertedMap ::Integer->String-> M.Map String Integer  
insertedMap str a= M.insert str a myMap 

newMap :: Integer->String->M.Map  Integer String
newMap a str = M.union (insertedMap a str)  newMap1 where newMap1=M.fromList[(100,"Hello")]             
--END
--}
--END
