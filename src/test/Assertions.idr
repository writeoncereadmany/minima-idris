module Assertions

%access export

assertEq : (Show a, Eq a) => (expected : a) -> (given : a) -> IO ()
assertEq expected given = if given == expected
    then putStrLn "Passed"
    else putStrLn $ "Failed: expected " ++ show expected ++ " but got " ++ show given

assertIsRight : (Show a) => (given : Either a b) -> IO ()
assertIsRight (Left l) = putStrLn $ "Failed: expected a Right, but got Left " ++ show l
assertIsRight (Right r) = putStrLn "Passed"

assertLeft : (Show a, Show b, Eq a) => (expected : a) -> (given : Either a b) -> IO ()
assertLeft expected (Right actual) = putStrLn $ "Failed: expected Left " ++ show expected ++ ", got Right " ++ show actual
assertLeft expected (Left actual) = if actual == expected
  then putStrLn "Passed"
  else putStrLn $ "Failed: expected " ++ show expected ++ ", got " ++ show actual

assertRight : (Show a, Show b, Eq b) => (expected : b) -> (given : Either a b) -> IO ()
assertRight expected (Left actual) = putStrLn $ "Failed: expected Right " ++ show expected ++ ", got Left " ++ show actual
assertRight expected (Right actual) = if actual == expected
  then putStrLn "Passed"
  else putStrLn $ "Failed: expected " ++ show expected ++ ", got " ++ show actual
