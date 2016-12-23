import System -- for usleep

-- test sth
stringToEachCode: String -> List Int
stringToEachCode str = unpack str >>= (\ch => [(ord ch)])

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

-- printLn' : Show a => a -> IO ()
-- printLn' x = putStrLn (show x)

readNumbers : IO (Maybe (Nat, Nat))
-- readNumbers =
--   do num1 <- readNumber
--      case num1 of
--           Nothing => pure Nothing
--           Just num1_ok =>
--                do num2 <- readNumber
--                   case num2 of
--                        Nothing => pure Nothing
--                        Just num2_ok => pure (Just (num1_ok, num2_ok))
readNumbers =
  do Just num1_ok <- readNumber | Nothing => pure Nothing
     Just num2_ok <- readNumber | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNumber
                  | Nothing => do putStrLn "Invalid input"
                                  countdowns
                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns
                             else pure ()

guess : (target : Nat) -> IO ()
guess target = do putStr "Guess number: "
                  Just num <- readNumber
                    | Nothing =>
                      do putStrLn "Invalid input"
                         guess target
                  case compare target num of
                    LT => do putStrLn "high"
                             guess target
                    GT => do putStrLn "low"
                             guess target
                    EQ => putStrLn "correct"
