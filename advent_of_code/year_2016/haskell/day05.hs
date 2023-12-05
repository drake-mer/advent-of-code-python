import Data.String
import qualified Data.List as List
import qualified Data.Map.Strict as HashMap
import qualified Data.Hash.MD5 as MD5

testInput = "abc"
puzzleInput = "cxdnnyjw"
prefix = "00000"

answer1 = map (!! 5) $ getResult1 $ getHashList puzzleInput
answer2 = getResult2 $ getHashList puzzleInput

getPasswordFrom :: [String] -> String
getPasswordFrom = appendTo HashMap.empty

appendTo :: HashMap.Map Char Char-> [String] -> String
appendTo myMap (x:xs) = if key `HashMap.member` myMap
    then appendTo myMap xs
    else appendTo (HashMap.insert key value myMap) xs
    where (key, value)=(x!!5, x!!6)
appendTo myMap [] = map snd  (take 8 (HashMap.toAscList myMap))

getResult1 :: [String] -> [String]
getResult1 = take 8 . filter ("00000" `isPrefixOf`)
getResult2 :: [String] -> [String]
getResult2 = take 40 . filter matchPreCondition

matchPreCondition :: String -> Bool
matchPreCondition x = and $ [ ("00000" `isPrefixOf'`), flip elem ['0'..'7'] . (!!5) ] <*> [x]

getHashList :: String -> [String]
getHashList input = map ( MD5.md5s . MD5.Str . (input ++) . show ) [0..]

isPrefixOf :: String -> String -> Bool
isPrefixOf (x:xs) (y:ys)
    | x==y =  xs `isPrefixOf` ys
    | otherwise = False
isPrefixOf (xs:_) [] = False
isPrefixOf [] _ = True

isPrefixOf' :: String -> String -> Bool
isPrefixOf' a b = a==take (length a) b

myAnswer = [ "000007c827126c81fa664211693f2540"
            ,"000007880153f1b804481a39a6d2e86a"
            ,"00000096164643e2e0fbf5a91bfd7f06"
            ,"000006ec13bc03b597beee4fa9352176"
            ,"00000426f1cd2f19a38114170b33c5ef"
            ,"000007d5ead928e4d0aa9dc516f16f42"
            ,"000007937c1b2a62ef8d18bab5f52a5d"
            ,"000002966111d9b5c057ab99802ff414"
            ,"000007a798e0c990883b19a55ce03092"
            ,"000003810329edb2a26e9ccf602fd5c1"
            ,"000006a809b3a491b43a2e3b9987dd91"
            ,"00000718147992fe5089c79072cb3b97"
            ,"000004ea66bd2ea0b40182aa607f8848"
            ,"000005829911142c7c1592863f4fd438"
            ,"0000042318414a15d88da98dd819a220"
            ,"000004cf633c6ad4fbe55232460c4dc6"
            ,"0000056f9e076bce2a2058086c415d39"
            ,"000003045ebaa06c1f1cb74f0c56bfec"
            ,"000004f24f1bb351f29a363bf61963bd"
            ,"0000019ba65feb3f3b74f37b29ce1481"
            ,"000004fb5c504e08e9ee2f9a679adc29"
            ,"000007e2be8a8305a2a880f656d5daf7"
            ,"00000745019fe9bd415453239220b1e7"
            ,"0000023847527648bab8f2e4006877d1"
            ,"0000034d867823a32225f6a9d72a87fd"
            ,"0000039527531a134d8fdb1014a4fb32"
            ,"000002b5a468a680b968cc57860d6f19"
            ,"000007d67617804e4781635c62151ba4"
            ,"000005ca5d3e4ee4b7a80a71ffa9dc47"
            ,"0000050d3a5439e6d3b39353d6c7ba83"
            ,"000005271971d854553ddd2c5bb232b1"
            ,"0000010f1bf50723c7493476ed05cb1c"
            ,"00000306bd136dda780c8fcc4a68dfd9"
            ,"0000047f40a7052d13267e0f82e62c35"
            ,"00000465c17d082a75ac11931b65d714"
            ,"000007b06e304a1c3285728872a6271a"
            ,"000007772db1f89c09cf823554966484"
            ,"000003b6f2e4a9e75ad5b960f2f8bf19"
            ,"00000523a9083e8f0d2791607b2d84af"
            ,"00000736d26b6a7e4b65d450c7911f50" ]
