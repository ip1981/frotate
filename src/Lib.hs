module Lib
  ( partition
  , partitionDays
  ) where

import Data.List (group, sortOn, span)
import Data.Ord (Down(Down))

import Data.Time.Calendar (Day, addDays, diffDays)

{- | Split a strictly increasing list of integer numbers into intervals
such that the width of each interval is defined by a positive, weakly increasing
function of the interval's serial number (1, 2, 3, ...).
Some of the intervals may be empty.

>>> partition (const 1) [1,2,3,4,5,6,7,8,9]
[[1],[2],[3],[4],[5],[6],[7],[8],[9]]

>>> partition (const 1) [1,3,5,7,9,11,13,15,17,19]
[[1],[],[3],[],[5],[],[7],[],[9],[],[11],[],[13],[],[15],[],[17],[],[19]]

>>> partition (const 2) $ take 10 [1,3 ..]
[[1],[3],[5],[7],[9],[11],[13],[15],[17],[19]]

>>> partition (\n -> floor (2^n)) $ take 20 [1, 2 ..]
[[1,2],[3,4,5,6],[7,8,9,10,11,12,13,14],[15,16,17,18,19,20]]

>>> partition (\n -> floor (2^n)) [2,5,14,20]
[[2],[5],[14],[20]]

>>> partition (\n -> floor (2^n)) $ take 20 [10,20 ..]
[[10],[],[20],[30],[40,50,60,70],[80,90,100,110,120,130],[140,150,160,170,180,190,200]]

-}
partition ::
     (Ord t, Num t)
  => (Int -> t) -- ^ positive, weakly increasing function of a natural number
  -> [t] -- ^ strictly increasing, non-empty list
  -> [[t]]
partition f l = go 1 (head l) l
  where
    go _ _ [] = []
    go n a aa =
      let a' = a + f n
          n' = n + 1
          (slot, rest) = span (< a') aa
       in slot : go n' a' rest

{- | Split days into intervals, backwards from the most recent day.
This function uses the 'partition' function after calculating differences
between the days.

>>> let days = map read ["2019-08-30", "2019-08-31", "2019-09-02"]
>>> partitionDays (const 1) days
[[2019-09-02],[],[2019-08-31],[2019-08-30]]

>>> let days = map read ["2019-08-30", "2019-08-31", "2019-09-01", "2019-09-02", "2019-09-03", "2019-09-04"]
>>> partitionDays (\n -> floor (2^(n-1))) days
[[2019-09-04],[2019-09-03,2019-09-02],[2019-09-01,2019-08-31,2019-08-30]]

-}
partitionDays ::
     (Int -> Integer) -- ^ positive, weakly increasing function of a natural number
  -> [Day] -- ^ non-empty list of days, with no other restrictions
  -> [[Day]]
partitionDays f days = map (map int2day) (partition f ints)
  where
    sorted = map head . group . sortOn Down $ days
    day1 = head sorted
    ints = map (diffDays day1) sorted
    int2day i = addDays (-i) day1
