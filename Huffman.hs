import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Binary
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as BIN
import System.IO

data Node = Node {frequency:: Float, symbol:: String, left:: Maybe Node, right:: Maybe Node}

instance Eq Node where
    (Node f1 _ _ _) == (Node f2 _ _ _) = f1 == f2

instance Ord Node where
    (Node f1 _ _ _) < (Node f2 _ _ _) = f1 < f2
    (Node f1 _ _ _) > (Node f2 _ _ _) = f1 > f2
    (Node f1 _ _ _) >= (Node f2 _ _ _) = f1 >= f2
    (Node f1 _ _ _) <= (Node f2 _ _ _) = f1 <= f2

instance Show Node where
    show (Node f s _ _) = show s ++ ": " ++ (show f)

myNode :: Float -> String -> Maybe Node -> Maybe Node -> Node
myNode f s l r = Node f s l r

mergeLeaves :: Node -> Node -> Node
mergeLeaves left right = myNode ((frequency left) + (frequency right)) "" (Just left) (Just right)

insertNode :: Node -> [Node] -> [Node]
insertNode x [] = [x] 
insertNode x (y:ys) = if x <= y then x:y:ys else y : insertNode x ys

buildTree :: [Node] -> [Node]
buildTree symbols = (drop 2 (insertNode (mergeLeaves (symbols!!0) (symbols!!1)) symbols))

checkBuilded :: [Node] -> Bool
checkBuilded nodes = (length nodes == 1)

goLeft :: Node -> Map.Map String [Bool] -> [Bool] -> Map.Map String [Bool]
goLeft root codes curCode = case (left root) of
             Just a -> calculateCodes codes a (curCode ++ [False])
             Nothing -> Map.insert (symbol root) curCode codes

goRight :: Node -> Map.Map String [Bool] -> [Bool] -> Map.Map String [Bool]
goRight root codes curCode = case (right root) of
             Just a -> calculateCodes codes a (curCode ++ [True])
             Nothing -> Map.insert (symbol root) curCode codes 

calculateCodes :: Map.Map String [Bool] -> Node -> [Bool] -> Map.Map String [Bool]
calculateCodes codes root curCode = goRight root (goLeft root codes curCode) curCode

main = do
 args <- getArgs
 let inFile = head args
 let outFile = args!!1
 content <- readFile inFile
 let unique = nub content
 let totalNumber = fromIntegral (length content)
 let counts = [fromIntegral ((length . filter (== c)) content) / totalNumber | c <- unique]
 let symbols = [myNode f [s] Nothing Nothing | (f, s) <- (zip counts unique)]
 putStrLn (show symbols)
 let root = head (until checkBuilded buildTree symbols)
 let codes = calculateCodes Map.empty root []
 let encoded = concat [Map.findWithDefault [] [c] codes | c <- content]
 --let compressed = putList encoded



 putStrLn (show encoded)
 let compressed = encode (encoded :: [Bool])
 putStrLn (show compressed)
 --let final = get compressed
 --h_out <- openFile outFile WriteMode
 --BIN.hPut h_out final
 --hClose h_out

 f <- openFile outFile WriteMode
 BIN.hPut f compressed
 hClose f


