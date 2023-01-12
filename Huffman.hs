import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Binary
import qualified Data.ByteString.Lazy as BIN
import System.IO
import Data.Int

-- Структура для хранения узла дерева
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

-- Создание родительского узла для двух листьев

mergeLeaves :: Node -> Node -> Node
mergeLeaves left right = myNode ((frequency left) + (frequency right)) "" (Just left) (Just right)

-- Вставка нового узла с сохранением отсортированности

insertNode :: Node -> [Node] -> [Node]
insertNode x [] = [x] 
insertNode x (y:ys) = if x <= y then x:y:ys else y : insertNode x ys

-- Одна итерация построения дерева

buildTree :: [Node] -> [Node]
buildTree symbols = (drop 2 (insertNode (mergeLeaves (symbols!!0) (symbols!!1)) symbols))

-- Построение завершено, когда останется лишь один лист

checkBuilded :: [Node] -> Bool
checkBuilded nodes = (length nodes == 1)

-- Рекурсивный обход дерева для формирования кодов

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

-- Разбиение массива bool на части размера 8

getChunks :: Int -> [a] -> [[a]]
getChunks _ [] = []
getChunks n l = (take n l) : (getChunks n (drop n l))

-- Формирование однобайтового числа из массива Bool

convertInt8 :: [Bool] -> Int8
convertInt8 [] = 0
convertInt8 (x : xs) = if x then 1 + 2 * convertInt8 xs else 2 * convertInt8 xs

main = do
 args <- getArgs
 let inFile = head args
 let outFile = args!!1

-- Нахождение частот всех символов
 content <- readFile inFile
 let unique = nub content
 let totalNumber = fromIntegral (length content)
 let counts = [fromIntegral ((length . filter (== c)) content) / totalNumber | c <- unique]
 let symbols = [myNode f [s] Nothing Nothing | (f, s) <- (zip counts unique)]

-- Построение дерева и формирование кодов
 let root = head (until checkBuilded buildTree symbols)
 let codes = calculateCodes Map.empty root []

-- Преобразование в байтовую строку
 let encoded = concat [Map.findWithDefault [] [c] codes | c <- content]
 let encoded_in_bytes = [convertInt8 b| b <- getChunks 8 encoded]
 let compressed = encode (encoded_in_bytes :: [Int8])

 f <- openFile outFile WriteMode
 BIN.hPut f compressed
 hClose f