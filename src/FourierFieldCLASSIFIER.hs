-- This module provides basic magic quadrants or mq-functions
-- purpose is to be used in file "src/Col..20.hs" 
-- by pg-functions
-- that have the duty to compare
--  input data ::  [String] 
-- with
-- mq-functions :: [[Int]]
--
module FourierFieldCLASSIFIER where
--import Colored_2_3_5_Counter20
import System.Random
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO

import UsefulFunctions19

root="C:/stack/forGlade/src"
takerleiN g h = (drop (g-1) (take g h))
-- PARTS BELOW ARE the LINES OF MQ6
-- ==> --there is supposed to be NO 6X6 panadiagonal MQ`s 
--just regular MQ6`s about ...? many ?

-- each of the [foleft2,foleft2Round,foright2,foright2Round,foright3]
-- will get a LINE as one of six functions below :

fofourier1MQ6 =  [ 1, 4,13,30,31,32]
fofourier2MQ6  = [35,34, 8,23, 9, 2]
fofourier3MQ6  = [18,15,17,26,16,19]
fofourier4MQ6 =  [27,14,28, 3,29,10] 
fofourier5MQ6  = [25,11,21,22,20,12]
fofourier6MQ6  = [ 5,33,24, 7, 6,36]

fofou1MQ6 = [1,2,3,34,35,36]
fofou2MQ6 = [4,17,28,29,12,21]
fofou3MQ6 = [10,27,30,5,20,19]
fofou4MQ6 = [31,24,9,22,11,14]
fofou5MQ6 = [32,16,23,8,26,6]
fofou6MQ6 = [33,25,18,13,7,15]

fopanfourier1MQ5 =  [0,8,11,19,22]
fopanfourier2MQ5  = [16,24,2,5,13]
fopanfourier3MQ5  = [7,10,18,21,4]
fopanfourier4MQ5 =  [23,1,9,12,15] 
fopanfourier5MQ5  = [14,17,20,3,6]

fopanfourier1MQ4 =  [0,7,10,13]
fopanfourier2MQ4  = [11,12,1,6]
fopanfourier3MQ4  = [5,2,15,8]
fopanfourier4MQ4 =  [14,9,4,3] 

fopanfourier1MQ3 =  [7,2,3]
fopanfourier2MQ3  = [0,4,8]
fopanfourier3MQ3  = [5,6,1]

chooseMQ n m = (drop (n-1)) (take n m)

fofourierRAW n fstOsnd mQs = (chooseMQ fstOsnd (concat (chooseMQ n mQs)))
fofourier6 n fstOsnd = fofourierRAW n fstOsnd [fofourier1MQ6 ,fofourier2MQ6 ,fofourier3MQ6 ,fofourier4MQ6 ,fofourier5MQ6 ,fofourier6MQ6,fofou1MQ6 ,fofou2MQ6 ,fofou3MQ6 ,fofou4MQ6 ,fofou5MQ6 ,fofou6MQ6 ]
fofourier5 n fstOsnd = fofourierRAW n fstOsnd [fopanfourier1MQ5 ,fopanfourier2MQ5 ,fopanfourier3MQ5 ,fopanfourier4MQ5 ,fopanfourier5MQ5]
fofourier4 n fstOsnd = fofourierRAW n fstOsnd [fopanfourier1MQ4 ,fopanfourier2MQ4 ,fopanfourier3MQ4 ,fopanfourier4MQ4 ]

------------------------------------------
-- active 28-8-2020 write WXmaxima 2d
functionalizeMQ3 n fstOsnd = fofourierRAW n fstOsnd [fopanfourier1MQ3,fopanfourier2MQ3,fopanfourier3MQ3]
fun3 n fstOSnd = functionalizeMQ3 n fstOSnd
ffourierMQ3 x n fstOSnd = ((head (map realToFrac (fun3  n fstOSnd)))*x)
fourierMQ3 n x =  show((sin(ffourierMQ3 x n 1) + (sin(ffourierMQ3 x n 2)) + (sin(ffourierMQ3 x n 3) )))
------------------------------------------

fourierRAW v = take v [1,2..]
ffRAW5 n m fstOsnd = (chooseMQ m (concat( map (fofourier5 n) (fstOsnd))))
ffRAW4 n m fstOsnd = (chooseMQ m (concat( map (fofourier4 n) (fstOsnd))))

fourier6RAW x y fstOsnd = (sin (( head (fofourier6 y fstOsnd ))*x))+(sin((head( fofourier6 y (fstOsnd+1)) )*x))  +  (sin((head( fofourier6 y (fstOsnd+2)))*x )) + (sin((head ( fofourier6 y (fstOsnd+3)))*x ))  +  (sin((head ( fofourier6 y (fstOsnd+4)))*x )) + ( sin ((last (fofourier6 y (fstOsnd+5)))*x)) 

fourier5RAW x n   = let toMap n m = (sin ((head(ffRAW5 n m [1..5]))*x))
                    in  map (toMap n) [1..5]
-- x:Int welche stelle
-- n:Int welche zeile
fourier4RAW x n   = let toMap n m = (sin ((head(ffRAW4 n m [1..4]))*x))
                    in  map (toMap n) [1..4] 
 

fourier1MQ6 x = fourier6RAW x 1 1
fourier1MQ5 x = sum (fourier5RAW x 1)
-- a Word 
fourier1MQ4 x = sum (fourier4RAW x 1)
fourier1TRACE4 n x = (sum (fourier4RAW (sum (fourier5RAW (fourier6RAW x n 1)n )) n))

fourier2MQ6 x =  fourier6RAW x 2 1
fourier2MQ5 x = sum (fourier5RAW x 2)
fourier2MQ4 x = sum (fourier4RAW x 2)

fourier3MQ6 (x) =  fourier6RAW x 3 1
fourier3MQ5 x = sum (fourier5RAW x 3)
fourier3MQ4 x = sum (fourier4RAW x 3)


fourier4MQ6 (x) =  fourier6RAW x 4 1
fourier4MQ5 x = sum( fourier5RAW x 4)
fourier4MQ4 x = sum (fourier4RAW x 4)


fourier5MQ6 (x) =   fourier6RAW x 5 1
fourier5MQ5 x = sum (fourier5RAW x 5)


fourier6MQ6 (x) =  fourier6RAW x 6 1

------------------------------------
--
fourier7MQ6 x =  fourier6RAW x 7 1

fourier8MQ6 x =  fourier6RAW x 8 1
fourier9MQ6 (x) =  fourier6RAW x 9 1

fourier10MQ6 (x) =  fourier6RAW x 10 1


fourier11MQ6 (x) =  fourier6RAW x 11 1

fourier12MQ6 (x) =   fourier6RAW x 12 1

fourierMQ6NOPAN123 (x) = (fourier1MQ6 x + fourier2MQ6 x + fourier3MQ6 x + fourier4MQ6 x + fourier5MQ6 x +fourier6MQ6 x )*(1/90)
fourierMQ5NOPAN123 x = (fourier1MQ5 x + fourier2MQ5 x + fourier3MQ5 x+ fourier4MQ5 x + fourier5MQ5 x)*(1/90)
fourierMQ4NOPAN123COLL x = let a fx = (fourier1MQ4 fx +fourier1MQ4 (fx+1) +fourier1MQ4 (fx+2) + fourier1MQ4 (fx+3))*(1/90)
                           --in let fo123 fx = (take fx ([1,4..]))
                           in a x --last(map a(fo123 x)) 

fourierMQ4NOPAN123ROW x = (fourier1MQ4 x)*(1/90)
fo1232 x = (fourierMQ4NOPAN123COLL x)
fourierMQ4NOPAN123 x = (fo1232 x)
--fourierMQ4POSITIVE x =   sum [(F.fourierMQ4NOPAN123ROW x),(F.fourierMQ4NOPAN123COLL x)]
 
fourierMQ4TRACE x =  ( (fourier1TRACE4 1 x)+ (fourier1TRACE4 2 x) + (fourier1TRACE4 3 x) + (fourier1TRACE4 4 x))*(1/90)
fourierMQ6NOPAN2 (x) = (fourier7MQ6 x + fourier8MQ6 x + fourier9MQ6 x + fourier10MQ6 x + fourier11MQ6 x +fourier12MQ6 x )*(1/90)




-- ORDER TO FIND METRIC that can redirect the weighing function
-- ----------------------------------------------------------------
-- check Parametric of each Line-- lts f dots
-- check 
readRandomSTREAM="1"

-- can make e.g 
-- takes e.g t=6 -> will yield 10 Sets of Random 
zufallsGen4 t a turn1 = (take t  (randomRs (1,99) (mkStdGen turn1)))::[Int]



-- READING an ALIEN PATH with "usefulFunctions19'
-- import ready file of KA-MACHINE
-- format:  e.g. "4.3453 c m 3.4556" wird zu ->
--                "4.3453"" 3.4556"["c", "m", "3.4556"]
-- exportiert 1. 
-- B) exportStream selectData tokenN <- Int String 
-- selectData: 1 fst number of string above
--             2 snd number "  "       "
--             3 fst letter 
--             4 sndletter  

readingAnyPATHFile = do
   putStrLn "File to read?"
   theAl <- getLine
 --  let foread= map show (map length ((map takerleiN (lines theAl)))
  -- let scansAl = show (mymonaD (unlines(words theAl)) 27)
 --  putStrLn (((scansAl)))
   putStrLn "1"-- (unlines foread)
