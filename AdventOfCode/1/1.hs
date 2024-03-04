--On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
--
--For example:
--
--1abc2
--pqr3stu8vwx
--a1b2c3d4e5f
--treb7uchet
--In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
--
import Text.Read (readMaybe)
import qualified Data.Maybe
import Data.Char

getNum:: Char -> Char
getNum c
    |isDigit c=c
    |otherwise ='\NUL'

selectNum::[Char]->[Char]->[Char]
selectNum r []=r
selectNum result (x:xs)=
  let resultMaybe=getNum x
      in case resultMaybe of
         '\NUL'->selectNum result xs
         _->selectNum (result++[resultMaybe]) xs

getCalibration request=
  let r=selectNum "" request
    in head r : [last r]

loop = do
  print "inserisci coordinate rotte"
  line<-getLine
  print (getCalibration line)
  loop

main = do
    loop
