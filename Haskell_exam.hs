---------------------------------1----------------------------------
--Вставить заданное число в список целых чисел: 
--а) после первого отрицательного 
--элемента; б) перед последним четным элементом

--f1 -- вставляем число после первого отриц. элемента
f1 :: Int -> [Int] -> [Int]
f1 n xs = foldr (\x ass -> 
 if x < 0 
  && not (null ass) 
   then x : n : ass else x : ass) [] xs
   
--f2 -- вставляем заданное число перед последним четным  
f2 :: Int -> [Int] -> [Int]
f2 n xs = let (before, after) = foldr (\x (befr, aftr) ->
                if even x && null aftr then (befr, n : x : aftr)
                else (x : befr, aftr)) ([], []) xs in before ++ after
---------------------------------2----------------------------------
--Переставить s-й элемент списка на место k-го элемента (s > k), 
--а к-й элемент на 
--место s-го.
--вообще нет идей как через свертку, реализовал примерно так же
--как и на лекциях реализовывали
f3 :: Int -> Int -> [a] -> [a]
f3 s k xs = f3_idhelp 1 xs
  where
    f3_idhelp _ [] = []
    f3_idhelp n (y:ys)
      | n == k = (xs !! (s-1)) : f3_idhelp (n+1) ys
      | n == s = (xs !! (k-1)) : f3_idhelp (n+1) ys
      | otherwise = y : f3_idhelp (n+1) ys
---------------------------вывод---------------------------------------
main = do
  let listok1 = [1,2,3,-4,5,6,7,8]
  -- s > k
  let s = 2
  let k = 4
  let s1 = 4
  let k1 = 8
  ----------
  putStrLn $ "БИЛЕТ"
  putStrLn $ "Первое задание"
  print $ f1 11 listok1
  print $ f2 11 listok1
  putStrLn $ ""
  putStrLn $ "Второе задание"
  print $ f3 s k listok1
  print $ f3 s1 k1 listok1