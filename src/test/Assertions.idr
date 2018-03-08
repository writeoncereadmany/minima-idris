module Assertions

%access export

assertEq : (Show a, Eq a) => (given : a) -> (expected : a) -> IO ()
assertEq given expected = if given == expected
    then putStrLn "Passed"
    else putStrLn $ "Failed: expected " ++ show expected ++ " but got " ++ show given

assertNothing : Show a => (given : Maybe a) -> IO ()
assertNothing Nothing = putStrLn "Passed"
assertNothing (Just x) = putStrLn $ "Failed: expected Nothing, but got " ++ show x

assertJust : (Show a, Eq a) => (expected : a) -> (given : Maybe a) -> IO ()
assertJust expected Nothing = putStrLn $ "Failed: expected " ++ show expected ++ ", got Nothing"
assertJust expected (Just actual) = if actual == expected
    then putStrLn "Passed"
    else putStrLn $ "Failed: expected " ++ show expected ++ ", got " ++ show actual

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
