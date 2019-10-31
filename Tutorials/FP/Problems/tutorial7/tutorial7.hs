-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

import LSystem
import Test.QuickCheck


pathExample1 = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)
-- pentagon 50
pathExample2 = (Go 50.0 :#: Turn 72.0 :#:Go 50.0 :#: Turn 72.0 :#:Go 50.0 :#: Turn 72.0 :#:Go 50.0 :#: Turn 72.0 :#:Go 50.0 :#: Turn 72.0)


-- 1a. split
split :: Command -> [Command]
split Sit = []
split (cmd1 :#: cmd2) = split cmd1 ++ split cmd2
split cmd = [cmd]

-- 1b. join
join' :: [Command] -> Command
join' []     = Sit
join' [x]    = x
join' (x:xs) = x :#: join xs
--join = undefined
join :: [Command] -> Command
join = foldr (:#:) Sit 

-- 1c. equivalent
-- equivalent 
equivalent :: Command -> Command -> Bool
equivalent  cmd1 cmd2 = split cmd1 == split cmd2 

-- 1d. testing join and split
-- prop_split_join 
prop_split_join :: Command -> Bool
prop_split_join  arbitaryCmd = join(split arbitaryCmd) == arbitaryCmd

prop_split_join' ::Command -> Bool
prop_split_join' cmd = equivalent (join (split cmd) ) cmd 
-- prop_split
-- prop_split :: Command -> Bool 
-- prop_split cmd = and[c| c <- split cmd , c /= Sit, c /= (:#:) ]
prop_split :: Command -> Bool
prop_split cmd = all f (split cmd)
    where
      f Sit       = False
      f (_ :#: _) = False
      f _         = True


-- 2a. copy
copy :: Int -> Command -> Command
copy n cmd | n <= 0 = Sit
           | n == 1 = cmd
           | n > 1 = cmd :#: copy (n-1) cmd

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon  d n = copy n (Go d :#: Turn a)
        where  a = 360 / (fromIntegral n)


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral d n s a = sp d n
  where
  sp d n | d <= 0 || n == 0 = Sit
         | otherwise        = Go d :#: Turn a :#: sp (d+s) (n-1)





-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = undefined

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined


-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined


-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo = undefined

main :: IO ()
main = display pathExample1
