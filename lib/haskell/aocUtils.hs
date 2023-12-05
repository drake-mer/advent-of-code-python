module AocUtils where


readInput' :: String -> IO [String]
readInput' fname = do
  content <- readFile fname
  let result = lines content
  return result


readInput :: IO [String]
readInput = readInput' "input.txt"
