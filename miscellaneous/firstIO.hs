-- from pulp fiction
main = do
   putStrLn "Say what again!"
   what <- getLine
   if what == "what?"
      then do putStrLn "shoot pistol"
      else do putStrLn "I dare you!"
              main

