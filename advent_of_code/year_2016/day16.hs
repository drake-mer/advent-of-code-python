input = "10111011111001111"
disk_size1 = 272
disk_size2 = 35651584

step :: String -> String
step foo = foo ++ "0" ++ (transform (reverse foo))

transform :: String -> String
transform = map (\x-> if x=='0' then '1' else '0')

getData :: Int -> String -> String
getData maxSize input
    | (length input) > maxSize = (take maxSize input)
    | otherwise = getData maxSize (step input)

hashSum :: String -> String
hashSum input
    | even (length input) = hashSum (reduce input)
    | otherwise = input

reduce :: String -> String
reduce (a:b:xs)
    | a==b = '1':(reduce xs)
    | otherwise = '0':(reduce xs)
reduce [] = []

answer1 = hashSum (getData disk_size1 input)
answer2 = hashSum (getData disk_size2 input)
