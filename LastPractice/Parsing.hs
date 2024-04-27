module Parsing where
import Data.List (intercalate)
import Data.Char (toUpper)

type Field = String
type Row = [Field]
type Table = [Row]

{-
Exercise 3.1.1
Write a function parseTable :: [String] -> Table that parses a table
represented in its concrete syntax as a list of strings (each corresponding to a single line
in the input) into its abstract syntax. (Hint: use the function words from the Prelude.)
-}

{-
ghci> parseTable ["hola","como","estas"]
[["hola"],["como"],["estas"]]
-}

parseTable :: [String] -> Table
parseTable [] = []
parseTable xs = map words xs

{-
Exercise 3.1.2. Write a function printLine :: [Int] -> String that, given a list of
widths of columns, returns a string containing a horizontal line. For example, printLine [5, 6, 6, 6]
should return the line "+-----+------+------+------+". (Hint: use the function
replicate.)
-}

{-
ghci> printLine [5,6,6,6]
"+-----+------+------+------+"
-}

printLine :: [Int] -> String
printLine n = concatMap styleLine n ++ "+"

styleLine :: Int -> String
styleLine n = "+" ++ replicate n '-'

{-
Exercise 3.1.4. Write a function printRow :: [(Int, String)] -> String that, given a
list of pairs—the left element giving the desired length of a field and the right element
its contents—formats one row in the table. For example,
printRow [(5, "Alice"), (6, "Allen"), (6, "female"), (6, "82000")]
should return the formatted row
"|Alice|Allen |female| 82000|"
(Hint: use the functions intercalate, map and uncurry.)
-}


{-
ghci> printRow [(1,"Anton"),(2,"Marisol"),(3,"Poma")]
"|Anton|Marisol|Poma|"
-}


printRow :: [(Int, String)] -> String
printRow fields = "|" ++ intercalate "|" (formFields fields) ++ "|"

formField :: (Int, String) -> String
formField (width, content) = content ++ replicate (width - length content) ' '

formFields :: [(Int, String)] -> [String]
formFields = map formField
{-
Exercise 3.1.6. Write a function printTable :: Table -> [String] that pretty prints the
whole table. (Hint: use the functions map, toUpper and zip.)
-}


{-
ghci> printTable [["row1","row1"],["row2","row2"]]
["+----+----+","|ROW1|ROW1|","+----+----+","|row2|row2|","+----+----+"]
-}

printTable :: Table -> [String]
printTable table =
    let (cells:rows) = table
        columnWidths = map maximum (transpose (map (map length) table))
        headerRow = headRow cells columnWidths
        separatorLine = printLine columnWidths
        dataRows = dataRow rows columnWidths
    in [separatorLine, headerRow, separatorLine] ++ dataRows ++ [separatorLine]

{-
ghci> dataRow [["head1","head2"],["ho","la"]] [1,2]
["|head1|head2|","|ho|la|"]
-}

dataRow :: [Row]->[Int] -> [String]
dataRow [] _ = []
dataRow (row:rows) columnWidths = printRow (zip columnWidths row) : dataRow rows columnWidths

{-
ghci> headRow ["hola","ho"] [1,2]
"|HOLA|HO|"
-}

headRow :: Row -> [Int] -> String
headRow r col = printRow (zip col (map (map toUpper) r))

-- transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]] ->  [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

main :: IO ()
main = do
    contents <- readFile "Data.txt"
    let linesOfFile = lines contents
    writeFile "output.txt" (unlines (printTable(parseTable linesOfFile)))
