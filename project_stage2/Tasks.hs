
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Read

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

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
get_avg_steps_per_h m = [["H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17"], get_table (transpose(tail m))]
                where
                    get_table m = medLine m 2:medLine m 3:medLine m 4:medLine m 5:medLine m 6:medLine m 7:medLine m 8:medLine m 9:[]

-- get the med of the values for given line
medLine :: Table -> Int -> Value
medLine t line = printf "%.2f" (foldr (\x acc -> to_float x + acc) 0 (head (drop (line - 1) t)) / fromIntegral (length (head (drop (line - 1) t))) :: Float)

-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = [["column","range1","range2","range3"], line "VeryActiveMinutes" 4 (transpose (tail m)), line "FairlyActiveMinutes" 5 (transpose (tail m)), line "LightlyActiveMinutes" 6 (transpose (tail m))]
                    where
                        line name l m = [name, sumLine m l 0 50, sumLine m l 50 100, sumLine m l 100 500]
-- returns sum of elements in line between the 2 limist
sumLine t line limitD limitU = show (foldr (\x acc -> if to_int x >= limitD &&  to_int x < limitU then acc + 1 else acc) 0 (head (drop (line - 1) t)))

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
            where op row acc = [head row, diffprint row, averageprint (drop 4 (reverse (tail row))), averageprint (drop 5 row)] : acc
                  averageprint l = printf "%.2f" (average l)
                  diffprint l = printf "%.2f" (diff (average(drop 5 l)) (average(drop 4 (reverse (tail l)))))
                  average l = foldr (\x acc -> to_float x + acc) 0 l/4
                  diff a b = if a > b then a - b else b - a

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



{-
    TASK SET 2
-}

-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = (head table):sort_by_column (get_column_number column (head table) 0) (tail table)
                   where
                       -- sorts table
                        sort_by_column poz table = sortBy (\row1 row2 -> if element poz row1 == element poz row2 then
                            compare (head row1) (head row2) else
                            if (readMaybe (element poz row1) :: Maybe Float) == Nothing then
                            compare (element poz row1) (element poz row2) else
                            compare (to_float (element poz row1)) (to_float (element poz row2))) table

-- returns the element of the row from column numer 'poz'
element poz row = head (drop poz row)

-- returns the column number with the desired name
get_column_number :: ColumnName -> Row -> Int -> Int
get_column_number name (x:xs) poz
                            | name == x = poz
                            | otherwise = get_column_number name xs (poz + 1)

-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = if (head t1) == (head t2) then t1 ++ (tail t2) else t1

-- Task 3
-- if one table is shrter, adds empty rows to it
hunion :: Table -> Table -> Table
hunion t1 t2
  | length t1 > length t2 = combine t1 (padded_table (length t1 - length t2) t2) []
  | length t1 < length t2 = combine (padded_table (length t2 - length t1) t1) t2 []
  | otherwise = combine t1 t2 []
  where
      -- combines each row of the 2 tables
      combine [] _ acc = acc
      combine t1 t2 acc = (head t1 ++ head t2) : combine (tail t1) (tail t2) acc

-- returns a table with aditional 'len' number of empty rows
padded_table :: Int -> Table -> Table
padded_table 0 t = t
padded_table len t = padded_table (len - 1) (t ++ [padded_row (length (head t)) []])

-- returns a row containing only empty strings with a given number or columns
padded_row :: Int -> Row -> Row
padded_row 0 row = row
padded_row len row = padded_row (len - 1) ("":row)

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = buildTable t1 t2 (commonColumn (head t1) (head t2) []) (header (head t1) (head t2) (commonColumn (head t1) (head t2) []))
            (get_column_number key_column (head t1) 0) (get_column_number key_column (head t2) 0) []

-- for each row of table 1, builds the new row
-- if the key_value doesnt exist in table t2, the row is skipped
buildTable [] _ _ hs _ _ acc = acc
buildTable (x:xs) t2 com hr poz1 poz2 acc = if keyRow poz2 (element poz1 x) t2 /= [] then
    buildTable xs t2 com hr poz1 poz2 (acc ++ [buildRow x (keyRow poz2 (element poz1 x) t2) com hr (head t2) []]) else
    buildTable xs t2 com hr poz1 poz2 acc

-- gets the 2 rows containing the common value, the list of common columns,
-- the header of the new table, the header of table 2 and an accumulator
-- returns the new row
buildRow :: Row -> Row -> Row -> Row -> Row -> Row -> Row
buildRow _ _ _ [] _ acc = acc
-- no more values in row1 to add, adds all the values from row2
buildRow [] row2 _ (hr:hs) hr2 acc = buildRow [] row2 [] hs hr2 (acc ++ [element (get_column_number hr hr2 0) row2])
buildRow (x:xs) row2 com (hr:hs) hr2 acc = if inList hr com then
                if element (get_column_number hr hr2 0) row2 == [] then
                    buildRow xs row2 com hs hr2 (acc ++ [x])
                else buildRow xs row2 com hs hr2 (acc ++ [element (get_column_number hr hr2 0) row2])
            else buildRow xs row2 com hs hr2 (acc ++ [x])

-- returns the row which contains 'value' on the column number 'poz' from table
keyRow poz value = foldr (\x acc -> if element poz x == value then x else acc) []

-- returns the new header
header r1 r2 com = foldl (\acc b -> if not (inList b com) then acc ++ [b] else acc) r1 r2

-- returns a list of common columns
commonColumn :: Row -> Row -> Row -> Row
commonColumn [] _ acc = acc
commonColumn (name:xs) r2 acc = if foldl (\existent el -> (el == name) || existent) False r2 then
                                commonColumn xs r2 (acc ++ [name]) else commonColumn xs r2 acc

-- returns True if element is in list
inList :: Eq t => t -> [t] -> Bool
inList key = foldr (\ x -> (||) (key == x)) False

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names:apply new_row_function (tail t1) (tail t2)

-- applyies f between all and each row of both tables
apply :: (Row -> Row -> Row) -> Table -> Table -> Table
apply f (x:xs) t2
                | xs == [] = map (f x) t2
                | otherwise = map (f x) t2 ++ apply f xs t2

-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose (extract columns_to_extract (transpose t) [])

-- extracs the given rows from table
extract :: [ColumnName] -> Table -> Table -> Table
extract [] _ acc = acc
extract (x:xs) t acc = extract xs t (acc ++ [findRow x t])

-- finds a row by first column
findRow _ [] = []
findRow col (x:xs) = if head x == col then x else findRow col xs

-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : foldr (\r acc -> if condition (element (get_column_number key_column (head t) 0) r) then r:acc else acc ) [] (tail t)
