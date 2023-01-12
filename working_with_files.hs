import System.Environment
import System.FilePath.Posix(takeBaseName)
import System.IO

writeToFile :: Handle -> String -> [String] -> IO ()
writeToFile file path args = do
 hPutStrLn file $ "This is a in file " ++ path ++ " invoked with\n" ++ (show args)

main :: IO ()
main = do
 putStrLn "Enter a string"
 s <- getLine
 putStrLn $ reverse s
 p <- getExecutablePath  
 a <- getArgs
 let fileName = head a
 f <- openFile fileName WriteMode
 writeToFile f p a
 hClose f
