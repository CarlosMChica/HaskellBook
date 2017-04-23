module EmployeeRank where

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank isBossOf e e' =
  case e `isBossOf` e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> reportBoss e' e

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

