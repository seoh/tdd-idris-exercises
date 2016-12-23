module Main

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

printLength : IO ()
-- printLength = getLine >>= \input => let len = length input in putStrLn (show len)
printLength = putStr "Input string: " >>= \_ =>
              getLine >>= \input =>
              let len = length input in
              putStrLn (show len)

printTwoThings : IO ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printInput : IO ()
printInput = do x <- getLine
                putStrLn x

printLonger : IO ()
printLonger = do putStrLn "First string: "
                 fst <- getLine
                 putStrLn "Second string: "
                 scd <- getLine
                 let maxLen = max (length fst) (length scd)
                 putStrLn $ show maxLen

printLonger' : IO ()
printLonger' = putStr "First string: " >>= \_ =>
               getLine >>= \fst =>
               putStr "Second string: " >>= \_ =>
               getLine >>= \scd =>
               let maxLen = max (length fst) (length scd) in
               putStrLn $ show maxLen
