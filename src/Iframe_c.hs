module Iframe_c (
    iframe_c
  , iframe_cWORK
  , criteria
  , htmlToken
    ) where
-- hackage
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
-- own 
import qualified Colored_2_3_5_Counter20 as Co
import qualified DataTypeBayes as BT 
import qualified UsefulFunctions19 as Us
import DataTypePunkt
import qualified HoofdDev as Hd 

mac a b = map a b 
twist io = if io==1 then [mother]
           else if io==2 then [father] 
           else if io==3 then [mother2] 
           else if io==4 then [loopNumber] 
           else if io==5 then [minMaxTrueOrFalse] 
           else []

-- bootPunkt: randomly generated cell names
--e.g  [["A","A","C","C","C","E"],["E","G","G","I","I","I"], ..]
rndCells foli fopi = (Hd.formHoofdEX1WORK [mother] foli ["CEL"] fopi "JJJJ" 2 3)
-- e.g fillCellempty "00000" "cell" "cell" 
fillCellempty newName g domain cellNames = BT.nameNewCell newName g domain cellNames

pi0 = Punkt "unsort" Nothing Nothing Nothing Nothing Nothing
pi1 = Punkt "intern" Nothing Nothing Nothing Nothing Nothing
liM3 = ["===========","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","01111","xyz=11"]
liM2 = ["00000","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","01111","xyz=11"]
--bootnewcell a b foli fopi = fillCellempty "00000" a b [(rndCells foli fopi)]
--mothersPaternalGrandfather s = richtungHoeheY s [mother, father, father]
-- maternals s = richtungHoeheY s (concat$ map twist o )
avanti e =  mapM_ putStrLn $ e

-- reconstruct rated input from formHoofdEX1
reconListRAW divBy foRated =  let step1 = foRated -- (usage 2)  
           in let step2 =   (length step1) `div` divBy  
           in let toRead =   if step2==0 then 1 
                             else step2 
           in let switch a  =  (drop ((a-1)*divBy)) $ (take (a*divBy)) step1 
              in mac switch [1..(toRead)]

reconList foRated = reconListRAW 6 foRated

--listOfGuesses: list
--b: list of same type as above
findIt b g = let tak = minimum(b)
         in let step1 g b = tak `elemIndices` b
       --  in let step2 b g = (map (step1 g ) b)  
       --  in let stepMap b = (map (step2 b) [1..(length(somethingTo b))])
         in ((b\\ [tak]) ,(step1 g b)) --ak --map stepMap b
  where
    somethingTo bereckoned = reconList bereckoned;

checkflow = BT.checkBayes
--iframe_c :: IO()
iframe_c mode = (iframe_cWORK "2" "2" "111" 1 1 "<p>" "wd" (show$Co.tsRAW Co.ptc9) mode "4")
-- dobinne:Int ,if ==1 then ONLY insert aCt0 else insert from 'formHoofdEX1WORK'
--same as above no getLine in GUI all top level
--t1: num String if 1 then show CELLS else select function
--t2: num String select function  1..9
--t3: numString if == "1" then transpose 9 functions over six variables (li list) else print the same without transpose
iframe_cWORK t1 t2 t3 c1 c2 aCt0 aCt foptc mode c3 dobinne = do
  --iO <- t1
  let prep foguess r line atom =  (Hd.formHoofdEX1WORK [mother] liM2 ["CEL"] pi0 foguess (read t1) r line atom)
  let toHT foWay otherWay =  Co.iframe_c c1 c2 aCt0 aCt [(show foWay)++"\n\n"++(show otherWay)++"\n\n" ++(foptc)++"\n"] mode c3

  if t1 == "1" then do 
         putStrLn "show solution matrix"
         foTH <- (prep t1 (read t2) 1 9)
         toHT foTH ("<p>")
         putStrLn ""
         --return ([""])
  else do   
    foprep <- forM [1..9] (\fp -> do
              inppp <- (prep t3 (read t2) fp 1)
              return (inppp))
    
 -- break prep into one String and reform the lists
 -- t: Int ; which of the rated values to choose
    let df t= words $ Us.replaceColon $ map chr $ (filter (<90) $ map ord (unwords(Us.tk t foprep)))
   -- avanti$map  show$ map  (reconList) (map df [1..9])
    avanti$map  show$map nub(map df [1..6])
    putStrLn " process rater functions of 'formHoofdEX1WORK'"
 -- get rid of one maxima for a change to get rid of most different stuff :|
    let raterRAW fodf = (map nub(fodf))
    let newWay = if t3== "1" then transpose $map sort (raterRAW (map df [1..6]))
                 else let com = if aCt == "wdc" then (map lines(comment (read t2))) ++(raterRAW (map df [1..6]))
                                else (raterRAW (map df [1..6]))
                      in com 
    let raters fodf = map minimum (map nub(fodf))
    let fg fodf = map show$ snd(findIt (raters fodf) 1)
    --putStrLn$show raters
    --putStrLn$show $ sort raters
    let dropSoff fodf = sort (raters fodf)
    let dsRAW fodf =  ((concat(map nub fodf)) \\ (dropSoff fodf)) --reconListRAW ((length $raterRAW (df 1))-1) ((concat(map nub fodf)) \\ (dropSoff fodf))
   -- let iterRR r = take 3 $concat$iterate dsRAW (map df r)
    let ds = dsRAW (map df [1..6])
  --  let anIter = iterate
    --let dopS l = raters \\ (take l dropSoff)
    let fg i =   (fst (findIt i 1))
    let iteR = (raters (map df [1..6]))
    
    let withI = (concat$raterRAW (map df [1..6]))\\ iteR -- reconListRAW (3) iteR -- \\ [(dropSoff (map df [1..6]))]
    let railout = if dobinne == 1 then toHT [("filesystem"++c3++".html")] foptc -- (raterRAW (map df [1..6])++[iteR])
                  else let comm = if aCt=="wdc" then toHT newWay (map lines(comment (read t2)) ++ raterRAW (map df [1..6])++[iteR])
                                  else toHT newWay (raterRAW (map df [1..6])++[iteR])
                       in comm
    railout
    putStrLn$show newWay --(raterRAW (map df [1..6]))--ds --withI --iteR2 --ds --ithI --iteR --raters --(dopS 2)

comment e = if e == 2 then [" 'compareCells' in 'formHoofdEX1WORK'\n"++
                          " compare booted cells: compare all of li to all of 'bootPunkt'\n"++ -- better [1..6] ?
                          " ausw 1 'map bootPunkt [1..10] ' => \n"++
                          " => [\"BBBDDF\",\"FFHHJJ\",\"JLLNNP\",\"PPRRTT\",\"TVVXXX\",\"ZZZ\\\\^\"]\n"++
                          " one by one of li compared to one of above\n"]
            else if e== 3 then comment 2
            else if e== 8 then ["'compareCells2' in 'formHoofdEX1WORK'\n"++
                          "  compare content in cells : compare one of li to a random char filled cell in 'rollOut'\n"++
                          "  one of li compared to cell content\n"]
           else [""]

rndM n =  last (Us.zufallsBasic1 (n+1) 9 n)

--crit:Int ; when stop to loop
--thisFunc: functions to output
-- Only 'simiVals' are func: 2 , 6 , 8 , 10   
--      'cell content'  are: 5 , 7 , 9          
--      'cell names'    are:  1 3 is any char, 4 insert "cell"
-- e.g> criteria "1" "1" "3" 
criteria t3 crit thisFunc = do
    let loop = do
         let out = thisFunc -- = head$Co.ausw (read n) thisFunc
         let clickLet n k= if n == 1 then "<p>"
                             else "<"++(htmlToken k)++">" 
      -- t3: Int ausw t2 [Solutions] , a list of a [list];
      -- n: Int start if == 1 then start at <p> and increment letter to <h> ...
      -- k: String start with "p" ; foPr: String token to write html e.g "1"
         let theCells func n k foPr = (iframe_cWORK "2" func t3 1 1 (clickLet n k) "wd" ( "can do this?\n"++ (clickLet (n+1) (htmlToken k) )) 2 foPr 2) 
         --let guesseS =   --liM3 n -- as four bread cells plug into breedCells
        -- let deltIn = round $ out-crit
         if out> crit then do 
                prod <- forM [1..3] (\pr -> do
                let fromCell = show$head$Co.ausw pr [2,6,8]
                if pr==1 then theCells fromCell pr (Co.ausw (1) "p") (show pr)
                else if pr==2 then theCells fromCell pr (Co.ausw (15+pr) "abcdefghijklmnopqrstuvwxyz") (show (pr+1))
                else theCells fromCell pr (Co.ausw (15+pr) "abcdefghijklmnopqrstuvwxyz") (show (pr+2))
                return (""))
               -- loop
                putStrLn "way 1"
         else do putStrLn "donde" --return ["done"]
    loop

htmlToken k = let step1 = map ord k
              in if (head step1) >= 122 then head$words$[chr 97]
                 else head$words$[chr$((head (step1)) + 1)]



