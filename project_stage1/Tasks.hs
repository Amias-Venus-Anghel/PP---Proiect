-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"]:write_table_steps (tail m)
    where write_table_steps [] = []
          write_table_steps ((x:xs):ys) = (x:(get_average_steps xs)):write_table_steps ys
          get_average_steps list = [printf "%.2f" (foldr (\x acc -> to_float x + acc) 0 list/8)]


-- auxiliar functions for convertion
to_float x = read x :: Float
to_int x = read x :: Integer


-- Task 2
total_steps :: [String] -> Integer
total_steps = foldr (\x acc -> to_int x + acc) 0

-- Number of people who have achieved their goal:)
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr (\(x:xs) acc -> if total_steps xs >= 1000 then acc + 1 else acc) 0 (tail m)

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m =  (fromIntegral (get_passed_people_num m) :: Float) / fromIntegral (length m - 1) :: Float


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = foldr op 0 (tail m) / fromIntegral (length m - 1) :: Float
                where op (_:xs) acumulator = foldr (\y acc -> to_float y + acc) acumulator xs
                      op [] acumulator = acumulator


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = [["H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17"], get_table (tail m)]
                where
                    get_table m = (medie1 m):(medie2 m):(medie3 m):(medie4 m):(medie5 m):(medie6 m):(medie7 m):(medie8 m):[]
                    medie1 m = printf "%.2f" (foldr (\(_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)
                    medie2 m = printf "%.2f" (foldr (\(_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)
                    medie3 m = printf "%.2f" (foldr (\(_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)
                    medie4 m = printf "%.2f" (foldr (\(_:_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m ) :: Float)
                    medie5 m = printf "%.2f" (foldr (\(_:_:_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m ) :: Float)
                    medie6 m = printf "%.2f" (foldr (\(_:_:_:_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)
                    medie7 m = printf "%.2f" (foldr (\(_:_:_:_:_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)
                    medie8 m = printf "%.2f" (foldr (\(_:_:_:_:_:_:_:_:x:_) acc -> to_float x + acc) 0 m/fromIntegral (length m) :: Float)

-- Task 4
get_activ_summary :: Table -> Table
get_activ_summary m = [["column","range1","range2","range3"], (veryactive (tail m)), (fairlyactive (tail m)), (lightlyactive (tail m))]

veryactive :: Table -> [String]
veryactive m = ["VeryActiveMinutes", show (foldr range1 0 m), show (foldr range2 0 m), show (foldr range3 0 m)]
           where range1 (_:_:_:x:_) acc = if to_int x < 50 then acc + 1 else acc
                 range2 (_:_:_:x:_) acc = if to_int x >= 50 &&  to_int x < 100 then acc + 1 else acc
                 range3 (_:_:_:x:_) acc = if to_int x >= 100 && to_int x < 500 then acc + 1 else acc

fairlyactive :: Table -> [String]
fairlyactive m = ["FairlyActiveMinutes", show (foldr range1 0 m), show (foldr range2 0 m), show (foldr range3 0 m)]
           where range1 (_:_:_:_:x:_) acc = if to_int x < 50 then acc + 1 else acc
                 range2 (_:_:_:_:x:_) acc = if to_int x >= 50 &&  to_int x < 100 then acc + 1 else acc
                 range3 (_:_:_:_:x:_) acc = if to_int x >= 100 && to_int x < 500 then acc + 1 else acc

lightlyactive :: Table -> [String]
lightlyactive m = ["LightlyActiveMinutes", show (foldr range1 0 m), show (foldr range2 0 m), show (foldr range3 0 m)]
           where range1 (_:_:_:_:_:x:_) acc = if to_int x < 50 then acc + 1 else acc
                 range2 (_:_:_:_:_:x:_) acc = if to_int x >= 50 &&  to_int x < 100 then acc + 1 else acc
                 range3 (_:_:_:_:_:x:_) acc = if to_int x >= 100 && to_int x < 500 then acc + 1 else acc

-- Task 5

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] :foldr (\(x:y:_) acc -> [x, y]:acc) [] (sort_by_second_collum m)

sort_by_second_collum m = sortBy op (tail m)
               where op  (n:x:_) (m:y:_) = if x == y then compare n m else compare (to_float x) (to_float y)
                     op _ _ = EQ


-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"]: rearenge (sort_by_second_collum (hDiference  m))

hDiference m = foldr op [] m
            where op row acc = [head row, diffprint row, averageprint (trunch(reverse (tail row))), averageprint (trunch (tail row))] : acc
                  averageprint l = printf "%.2f" (average l)
                  diffprint l = printf "%.2f" (diff (average(trunch (tail l))) (average(trunch(reverse (tail l)))))
                  average l = foldr (\x acc -> to_float x + acc) 0 l/4
                  diff a b = if a > b then a - b else b - a
                  trunch l  = tail(tail(tail(tail l)))

rearenge m = foldr op [] m
            where op (a:b:c:d:xs) acc = (a:c:d:b:xs):acc
                  op _ acc = acc



-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f (tail m))

get_sleep_total :: Row -> Row
get_sleep_total (x:xs) = x: (printf "%.2f" (total xs)):[]
               where total = foldr (\x acc -> to_float x + acc) 0
