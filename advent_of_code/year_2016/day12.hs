import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char
input_puzzle = [   "cpy 1 a",
    "cpy 1 b",
    "cpy 26 d",
    "jnz c 2",
    "jnz 1 5",
    "cpy 7 c",
    "inc d",
    "dec c",
    "jnz c -2",
    "cpy a c",
    "inc a",
    "dec b",
    "jnz b -2",
    "cpy c b",
    "dec d",
    "jnz d -6",
    "cpy 14 c",
    "cpy 14 d",
    "inc a",
    "dec d",
    "jnz d -2",
    "dec c",
    "jnz c -5"  ]


data State = State Program Memory Address
type Address = Int
data Instruction = Jnz Char Int | Dec Char | Inc Char | CpyVar Char Char | CpyInt Int Char 
    deriving (Read, Show)
type Program = [Instruction]
type Memory = Map.Map Char Int

instance Show State where 
    show (State p m a ) = (show m) ++ " instr: " ++ if a>length p - 1 then "out of bound" else show(p!!a) ++ ", at addr: " ++ show(a)

readInstruction :: String -> Instruction
readInstruction input
    | x=="cpy" && all(`elem`['0'..'9'])y = (CpyInt (read y) (head z))
    | x=="cpy" && all(`elem`['a'..'z'])y = (CpyVar (head y) (head z))
    | x=="dec" = Dec (head y)
    | x=="inc" = Inc (head y)
    | x=="jnz" = Jnz (head y) (read z)
    where [x,y,z] = splitInstruction input

splitInstruction :: String -> [String]
splitInstruction input 
    | length (words input) == 3 = words input
    | length (words input) == 2 = (words input) ++ [[]]

myprog :: State
myprog = initProg1 (map readInstruction input_puzzle)

apply :: Instruction -> State -> State
apply (CpyVar x y) (State p m a) = (State p (Map.insert y (m Map.! x) m) (a+1))
apply (CpyInt x y) (State p m a) = (State p (Map.insert y x m) (a+1))
apply (Inc x) (State p m a) = (State p (Map.adjust (+1) x m) (a+1))
apply (Dec x) (State p m a) = (State p (Map.adjust (\x->x-1) x m) (a+1))
apply (Jnz x val) (State p m a)
    | x `elem` ['1'..'9'] = (State p m (a+val))
    | x=='0' || (m Map.! x) == 0 = (State p m (a+1))
    | otherwise =  (State p m (a+val))

initProg2::Program->State
initProg2 p = (State p (Map.insert 'c' 1 $ Map.fromList (zip ['a'..'d'] (repeat 0) )) 0)

initProg1::Program->State
initProg1 p = (State p (Map.fromList (zip ['a'..'d'] (repeat 0) )) 0)


execProgramLive :: State -> State
execProgramLive (State p m a) 
    | a>(length p)-1 || a<0 = State p m a
    | otherwise = execProgramLive(execInstruction (State p m a))

execProgram :: [State] -> [State]
execProgram ((State p m a):foo)
    | a > length p - 1 || a<0 = (State p m a):foo
    | otherwise = execProgram ((execInstruction (State p m a)):(State p m a):foo)

execNInstruction :: Int -> [State] -> [State]
execNInstruction n ((State p m a):foo)
    | n==0 = ((State p m a):foo)
    | (a > length p - 1) = ((State p m a):foo)
    | otherwise = execNInstruction (n-1) ((execInstruction (State p m a)):(State p m a):foo)

execInstruction :: State -> State
execInstruction (State p m a) 
    | (a > length p - 1) || a<0 = State p m a
    | otherwise = apply (p!!a) (State p m a)
