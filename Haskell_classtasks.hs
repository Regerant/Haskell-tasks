--my playground https://play.haskell.org/saved/B9hlrnhe
import Data.Ratio
import Data.List (maximumBy)
--перемножение матриц
myLeng :: [a] -> Int
myLeng [] = 0
myLeng [a] = 1
myLeng (x:xs) = 1 + myLeng xs

multiI :: Num a => [[a]] -> [[a]] -> Int -> [[a]]
multiI a b i
 | i < (myLeng a - 1) = [(multiJ a b i 0)] ++ multiI a b (i+1)
 | otherwise = [multiJ a b i 0]

multiJ :: Num a => [[a]] -> [[a]] -> Int -> Int -> [a]
multiJ a b i j 
 | j < myLeng (b!!0) - 1 = [(multiK a b 0 i j)] ++ multiJ a b i (j+1)
 | otherwise = [multiK a b 0 i j] 
 
multiK :: Num a => [[a]] -> [[a]] -> Int -> Int -> Int -> a
multiK a b k i j
 | k < (myLeng b - 1) = (a!!i!!k) * (b!!k!!j) + multiK a b (k+1) i j
 | otherwise = (a!!i!!k) * (b!!k!!j)
 
multipil :: Num a => [[a]] -> [[a]] -> [[a]]
multipil x y
 | matrixcheck x y == False = error "uncorr"
 | otherwise = multiI x y 0
 
matrixcheck :: [[a1]] -> [[a2]] -> Bool
matrixcheck [] [] = False
matrixcheck x [] = False
matrixcheck [] y = False
matrixcheck(x:xs) (y:ys)
 | myLeng x == myLeng (y:ys) && checkrows (myLeng x) xs == True && checkrows(myLeng y) ys == True = True
 | otherwise = False
 
checkrows :: Int -> [[a]] -> Bool
checkrows lengX [] = True
checkrows lengX [x]
 | lengX /= myLeng x = False
 | otherwise = True
checkrows lengX (xs:xss)
 | lengX /= (myLeng xs) = False
 | otherwise = checkrows lengX xss
--транспонирование
transpose1 :: [[a]] -> [[a]]
transpose1 [] = []
transpose1 ([] : xs1) = transpose1 xs1
transpose1 ((x : xs) : xs1) = (x : [h | (h : _) <- xs1]) : transpose1 (xs : [ t | (_ : t) <- xs1])
--определитель матрицы
dely1 :: Int-> [a] -> [a]
dely1 1 (x:xs) = xs
dely1 k (x:xs) = [x] ++ dely1 (k-1) xs

deletesty::(Num a) => Int -> [[a]] -> [[a]]
deletesty k x = [c |c <- [dely1 k q | q<-x ]]

determinant :: Num a => [[a]] -> a
determinant [] = error "Empty Error"
determinant [[x]] = x
determinant (x:xs) = 
 sum [(-1)^(j+1) * (head [x])!!(j-1) 
  * determinant(deletesty j xs) | j<-[1..(length x) ]]
----------------------------------монады--------------------
type Matrix = [[Double]]

determinantmonad :: Matrix -> Double
determinantmonad [] = 1
determinantmonad [[x]] = x
determinantmonad mat = sum [(-1)^i * mat!!0!!i * determinantmonad (minor 0 i mat) | i <- [0..n-1]]
 where
  n = length mat
  minor row col mat = [[mat!!i!!j | j <- [0..n-1], j /= col] | i <- [0..n-1], i /= row]
---------------------------------
complementMatrix :: Num a => [[a]] -> [[a]]
complementMatrix x = [[(-1)^(i+j) * determinant(deletesty j (dely1 i x)) | j<-[1..len]] | i<-[1..len]]
    where
        len = length (x!!0)

inverseMatrix :: Integral a => [[a]] -> [[Ratio a]]
inverseMatrix x
    | determinant x == 0 = error "Inverse matrix does not exist"
    | otherwise = (map.map) (% determinant x) (transpose(complementMatrix x))
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose [[x]] = [[x]]
transpose x = [map head x] ++ transpose(map tail x) 
--------------------------------------------------------------
addoper :: (Num a) => (a -> a -> a) -> [[a]] -> [a]
addoper _ [] = []
addoper _ [x] = x
addoper op (x:xs) = foldl1 (zipi op) xs

zipi :: (a -> b -> c) -> [a] -> [b] -> [c]
zipi _ [] _ = []
zipi _ _ [] = []
zipi f (x:xs) (y:ys) = f x y : zipi f xs ys

--some shit
add :: Int -> Int -> Int
add x y = x + y

--some big shit dickest task
sumallin :: Num a => [a] -> a
sumallin = foldl1 (+)
---------------------------------------------------------------
bumbulshit :: Ord a => [a] -> [a]
bumbulshit list = foldr (\x xs -> bumbulshit x xs) [] list
  where
    bumbulshit x [] = [x]
    bumbulshit x (y:ys)
      | x <= y = x:y:ys
      | otherwise = y : bumbulshit x ys
---------------------------------------------------------------
--bigshit sotring(stone sort)
stoneshit :: Ord a => [a] -> [a]
--chouza ur shitsorting:

--1
stoneshit = foldr (\x xs -> insert x xs) []

--2
--stoneshit = go []
--  where
--    go sortd [] = sortd
--    go sortd (x:xs) = go (insert x sortd) xs

--3
--stoneshit = foldl (flip insert) [] 

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)

--from biggaz to littaz
--  | x >= y = x:y:ys

--from littaz to biggaz
  | x <= y = x:y:ys
  | otherwise = y : insert x ys
----------------------------------------------------------------
--fastasort
fasta :: Ord a => [a] -> [a]
fasta [] = []
fasta (x:xs) = 
  let (niggar, niggal) = foldr (\a (ss,bs) -> if a <= x 
      then (a:ss, bs) -- ass
      else (ss, a:bs)) ([], []) xs
        in fasta niggar ++ [x] ++ fasta niggal 
----------------------------------------------------------------
rank :: Matrix -> Int
rank = length.filter (not . all (== 0)) . gauss

gauss :: Matrix -> Matrix
gauss [] = []
gauss m = pivot : gauss (eliminate pivot m)
  where
    pivot = maximumBy (\row1 row2 -> compare (head row1) (head row2)) m
    eliminate r m = [zipi (-) row (map (* c) r) | row <- m, let c = head row / head r]
----------------------------------------------------------------
main=do
 let mat1 = [[1,2],[3,4]] --[[1,2],[3,4]]
 let mat2 = [[1,2],[1,2]] --[[2,3,4],[5,6,7]]
 let matrix = [[1,2],[3,4]]
 let matrixe = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
 
 let nums1 = [1,2,3]
     nums2 = [3,4,5]
     nums3 = [5,6,7,8,11,3,8,10,155]
 
 print $ "matrix:"
 print $ multipil mat1 mat2
 print $ inverseMatrix mat1
 print $ transpose1 mat1
 print $ determinant mat1
 print $ determinantmonad matrix
 print $ rank matrixe
 putStrLn $ ""
 
 print $ "sumallin:"
 print $ (sumallin (zipi add nums1 nums2))
 putStrLn $ ""
 
 print $ "sorts:"
 print $ nums3
 print $ (bumbulshit nums3)
 print $ (stoneshit nums3)
 print $ (fasta nums3)
 putStrLn $ ""