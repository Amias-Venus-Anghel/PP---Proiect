
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

import Common

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
-- repeat
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



{-
    TASK SET 3
-}


--3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

-- auxiliar functions are used to call the functions from older task sets, without
-- the packaghing of QRusult
instance Eval Query where
    eval (FromTable table) = Table table
    eval (AsList colname query) = List (get_column_as_row colname (eval query))
    eval (Sort colname query) = Table (sort_t colname (eval query))
        where
            sort_t name (Table t) = tsort name t
            sort_t _ _ = []
    eval (ValueMap op query) = Table (vmap_t op (eval query))
        where
            vmap_t f (Table t) = vmap f t
            vmap_t _ _ = []
    eval (RowMap op colnames query) = Table (rmap_t op colnames (eval query))
        where
            rmap_t op colnames (Table t) = rmap op colnames t
            rmap_t _ _ _ = []
    eval (VUnion query1 query2) = Table (vu_t (eval query1) (eval query2))
        where
            vu_t (Table t1) (Table t2) = vunion t1 t2
            vu_t _ _ = []
    eval (HUnion query1 query2) = Table (hu_t (eval query1) (eval query2))
        where
            hu_t (Table t1) (Table t2) = hunion t1 t2
            hu_t _ _ = []
    eval (TableJoin colname query1 query2) = Table (tj_t colname (eval query1) (eval query2))
        where
            tj_t colname (Table t1) (Table t2) = tjoin colname t1 t2
            tj_t _ _ _ = []
    eval (Cartesian op colnames query1 query2) = Table ( cart_t op colnames (eval query1) (eval query2))
        where
            cart_t op colname (Table t1) (Table t2) = cartesian op colname t1 t2
            cart_t _ _ _ _ = []
    eval (Projection colnames query) = Table (proj_t colnames (eval query))
        where
            proj_t colnames (Table t) = projection colnames t
            proj_t _ _ = []
    eval (Filter cond query) = Table (fil_t (eval query))
        where
            fil_t (Table t) = (head t) : foldr (\el acc -> if feval (head t) cond el then el:acc else acc) [] (tail t)
            fil_t _ = []
    eval (Graph edgeop query) = Table (make_graph edgeop (eval query))


-- returns column with name 'colname' as a row if QResult is a Table
get_column_as_row :: String -> QResult -> Row
get_column_as_row colname (Table t) = head (drop (get_column_number colname (head t) 0)  (transpose (tail t)))
get_column_as_row _ (List l) = l

--  3.4
-- by comparing by hand the result with the reference, it seems that the only difference is the order of the rows,
-- but no rule was given for the way they should be in and i can't figure one out by the referance alone
-- makes a graph table
make_graph :: EdgeOp -> QResult -> Table
make_graph edgeop (Table t) = ["From", "To", "Value"] : (filter_dubs (table_body (tail t)))
    where
        table_body :: Table -> Table
        table_body t = foldr (\row1 acc -> (foldr (\row2 acc2 -> if edgeop row1 row2 /= Nothing && row1 /= row2 then
            (sortBy compare ([head row1, head row2])++[val_maybe (edgeop row1 row2)]):acc2 else acc2) [] t ) ++ acc ) [] t

val_maybe (Just a) = a
-- returns a list without doublicates
filter_dubs l = foldr (\el acc -> if not (in_list el acc) && el /= [] then el:acc else acc) [] l
-- checks if element is in list
in_list _ [] = False
in_list searched l = foldr (\el acc -> el == searched || acc) False l

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- for each one, the value from the column and row given is evaluated
instance FEval String where
    feval names (Eq colname ref) = filop
        where
            filop row = head (drop (get_column_number colname names 0) row) == ref
    feval names (Lt colname ref) = filop
        where
            filop row = head (drop (get_column_number colname names 0) row) < ref
    feval names (Gt colname ref) = filop
        where
            filop row = head (drop (get_column_number colname names 0) row) > ref
    feval names (In colname list) = filop
        where
            filop row = filter (== head (drop (get_column_number colname names 0) row)) list  /= []
    feval names (FNot cond) = filop
        where filop row = not (feval names cond row)
    feval names (FieldEq colname1 colname2) = filop
        where
            filop row = head (drop (get_column_number colname1 names 0) row) == head (drop (get_column_number colname2 names 0) row)

instance FEval Float where
    feval names (Eq colname ref) = filop
        where
            filop row = to_float (head (drop (get_column_number colname names 0) row)) == ref
    feval names (Lt colname ref) = filop
        where
            filop row = to_float (head (drop (get_column_number colname names 0) row)) < ref
    feval names (Gt colname ref) = filop
        where
            filop row = to_float (head (drop (get_column_number colname names 0) row)) > ref
    feval names (In colname list) = filop
        where
            filop row = filter (== to_float (head (drop (get_column_number colname names 0) row))) list  /= []
    feval names (FNot cond) = filop
        where filop row = not (feval names cond row)
    feval names (FieldEq colname1 colname2) = filop
        where
            filop row = to_float (head (drop (get_column_number colname1 names 0) row)) == to_float (head (drop (get_column_number colname2 names 0) row))



-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5
similarities_query :: Query
similarities_query = undefined

-- 3.6 (Typos)
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = undefined
