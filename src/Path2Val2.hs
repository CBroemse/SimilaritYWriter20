-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

-- this module can transfer a path into the DreiBandDATA stream 
-- that way it can be put into the KAAnsatz random generator
module Path2Val2 
    (writePath2ValFile,
     writePath2ValLine,
     writePath,
     exportStream, -- which part of a PATH lin to read  { fst; char1; char2; snd } ?
     exportStreamRaw, -- same as above but can be given other dininspezall file
     --solang, -- a Global Variable that finds out how long a pathFile is by itself
     val2PatternFile,
     checkfad,  -- best SVG writer !!!
     searchLineLogicRaw,
    -- writeOptimGraphRaw, -- most versatile tool choose modi of extraction
   --  iterateSimu, -- write aakaLRun and loop it
     --alongRunNew, -- write optimizer file 
     aKA,
     root,
     avanti,
     zeitraum,
     zufallsBasic,
     rndLength,
     easyfoNowRun,
     takerOne)
     where
--zufallsBasic t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]

--rndLength = zufallsBasic 3 1000000000 10
--easyfoNowRun wieviele = zufallsBasic wieviele 5000 10
----------------------------------------------------------
--in short
-- Stream Data -> Run Simulation -> Write new SVG/xhtml
----------------------------------------------------------
--in detail
--STEP 1
-- exportStream Int (show Int) -> testInhalt1
--                             -> testSVGInhalt
--                             -> testInhalt3.txt the new svg path ready 4 export
--STEP 2
-- val2PatternFile -> IMPORT simulated Vals into a patternfile (val,day,month,year)
--                     so that the runKAGO Simulation from module DreiBandTEST 
--                     can be run in below aKA function
--                 -> writes "senior.txt"
--
--STEP 3
-- aKA f randm -> RUN SIMULATION 
--                f:string e.g"1" how many steps -> one step simulates 10 vals
--                randm:string e.g"100"
--             -> writes "aaKA/aaKAnsatz"++token++".txt"
--                       the simulated Vals
--
--STEP 4
--checkfad token -> filter nur die simulierten Vals aus (Val) style file
--                  naemlich "aaKA/aaKAnsatz"++token++".txt"
--               -> token:string e.g. "8"
--               ->  writeSVGFile liest testInhalt-> toxi.svg
--
--
--                             
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
import System.Random 
-- my modules
import UsefulFunctions19
import WriteWXmaximaJuicy
--import qualified Colored_2_3_5_Counter as Co hiding (mother,father,Point,mother2,minMaxTrueOrFalse,loopNumber,nACC,nACCRAW)
--import DreiBandTEST
--import MENZEdeveloper2
--import MyDataWriter2
--import DreiBandDATA
-- import SimpleJSON

root = "C:/stack/PixelGitJuicy/src/"
fosourceAlWall thar = (root++ thar ) -- e.g "Path2ValDATA/dininSpez1kompl.txt")  AlWalPathkompl
sourceAlWall = (root++  "fgt.txt") 
avanti e =  mapM_ putStrLn $ e
zeitraum = 370 --die Laenge des zu simulierenden Zeitraums in Tagen
takerOne as1 bs2 = head (drop (as1-1) (take as1 bs2))
-- 80  --Statistic
------------------------------------------------------------------------
--DATEN: vom typ [Double] einlesen und simulieren
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Programmverlauf (2) : 
-- Schritt 1: Lese eine AllWalPAthfile (der pfad aus einer Html/Svg Datei)
-- Bsp fuer einzelne Zeile , teil des Programmverlaufs aber funktioniert 
-- auch als eigenstaendige funktion
-- -> Ziel: Ein Pfad einer SVG datei besteht aus Paaren es wird der erste Wert 
-- eines solchen Paares e.g. "1.3445 c 5.6733"   genommen
--                            fst       snd    
-- simuliert -> in                               
-- A) writePath2ValFile selectLine-> zeige Zeile Int aus allwalpathfile
--                         -> schreibt "testInhalt.txt"
--          e.g. "4.3453 c m 3.4556" wird zu ->
--                "4.3453"" 3.4556"["c", "m", "3.4556"]
-- exportiert 1. 
-- B) exportStream selectData tokenN <- Int String 
-- selectData: 1 fst number of string above
--             2 snd number "  "       "
--             3 fst letter 
--             4 sndletter  

--step1 break criteria "," is char 44)
--will take everything BEFORE the first kommata
cvsZeilenBreak s = let a = map ord s  --ord '-'=45
                   in let teile = takeWhile (/=44) a
                   in teile

--acces to leerstelle: " " is char 32
cvsZeilenBreak2 s = let a = map ord (s)  --ord '-'=45
                    in let teile = takeWhile (/=32) a
                    in teile

--acces to first of ALL Letters capital and not: "A" is char 65
cvsZeilenBreakL s = let a = map ord (s)  --ord '-'=45
                    in let teile = takeWhile (<65) a
                    in teile


--everything except " : '"' is char 32 
cvsZeilenBreak32 s = let a = map ord (s)  --ord '-'=45
                    in let teile = takeWhile (/=32) a
                    in teile
--acces to 
cvsZeilenBreak3 s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (<=65) a
                    in teile

cvsZeilenBreak4 s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (>45) a
                    in teile

cvsZeilenBreak5 s = let a = map ord (s)  --ord '-'=45
                    in let teile = dropWhile (>=57) a
                    in teile

-- fuer Iteration zum Filtern einer 
-- ALLWalPath Style 
-- (String, apath of an txt file ready 2 be pasted into an svg file datei)
-- wird in writePAth2ValFile demString zeilen writer verwendet
writePath2ValFile0 nF=
     let anyWalk = nF --readFile "Path2ValDATA/dininSpez1kompl.txt" -- z.b. "mageWalAllPath.txt"--durble.csv
    
     in let auswahl      = let an = takerleiN 1 anyWalk
                           in concat (an)
       -- drop with first occurance if a ','
     in let chgAus= map chr (dropWhile (>44) (map ord ( auswahl))) 
          
     in let tag = let an =  cvsZeilenBreak ( auswahl)
               in map chr an

     in let foprocess = tag ++ ","
 
 
     in let process = ( auswahl) \\ show foprocess

                  --  let exxport = repeat process
     in let aparser =  tag ++"\n\n\n"++ process --""++"("++val++","++year++","++

  
     in   process 
------------------------------------------------------
-- teil des Programmverlaufs wird in exportStream benutzt
-- hilft bei iteration um buchstaben aus SVG style pfad auszulesen
-- neueauswahl String - pipe um schon glesene daten einzufuegen
writePath2ValFile1 nF nEueauswahl=
     let anyWalk = nF --readFile "Path2ValDATA/dininSpez1kompl.txt" -- z.b. "mageWalAllPath.txt"--durble.csv
   
     in let auswahl      = let an = takerleiN 1 anyWalk
                           in concat (an)
       -- drop with first occurance if a ','
     in let chgAus= map chr (dropWhile (>44) (map ord ( auswahl))) 
          
     in let tag = let an =  cvsZeilenBreak ( nEueauswahl)
               in map chr an

     in let foprocess = tag ++ ","
 
 
     in let process = ( auswahl) \\ show foprocess

                  --  let exxport = repeat process
     in let aparser =  tag ++"\n\n\n"++ process --
     in   tag --

----------------------------------------------------------------------

-- exports stp2 the Second part :
-- "1.45677 4.332355"
-- "  fst  vs  snd  "-part
-- stp4 a lettr 
fogetSnd fogetData fofstNum =
         let anyWalk = fogetData  
         in let auswahl = let an = takerleiN 1 anyWalk
                          in concat (an)
         in let stp1 = map ord fogetData
         in let stp2 = auswahl \\ fofstNum
         in let stp3 =  (cvsZeilenBreakL (fogetData))
         in let fostp4 = map chr (cvsZeilenBreak(map chr (cvsZeilenBreak3 (fogetData)))) --get letter
         in let stp4 = fostp4 \\(fofstNum) 
         in if stp3 >= [(ord 'A')] then fofstNum++(map chr stp3)
            else (stp2) --map chr stp3)

fogetChrs fogetData fofstNum =
         let anyWalk = fogetData  
         in let auswahl = let an = takerleiN 1 anyWalk
                          in concat (an)
         in let stp1 = map ord fogetData
         in let stp2 = auswahl \\ fofstNum
         in let stp3 =  (cvsZeilenBreakL (fogetData))
         in let fostp4 = map chr (cvsZeilenBreak(map chr (cvsZeilenBreak3 (fogetData)))) --get letter
         in let stp4 = fostp4 \\(fofstNum)
         in let fostp5 = (map chr (cvsZeilenBreak4 (fostp4)))
         in let stp5 = fostp4 \\ fostp5
         in let iterData = map chr(cvsZeilenBreak fogetData) 
         in (stp5,iterData) --stp5 --fofstNum++(map chr stp3)


-------------------------------------------------------------------
-- MainSelecor Function can select Data beteen Kommata within
-- a Path, can select up to max 2 letters within Kommata 
-- selectLine            
writePath2ValFile selectLine = do
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile "fgt.txt" --(sourceAlWall) -- z.b. "mageWalAllPath.txt"--durble.csv
     let leane = length anyWalk
     let whereSpaces = let x1 =  (takerleiN 1 anyWalk)
                       in let x2 = ',' `elemIndices` (concat x1)
                       in x2 
    
     let bobbo =  ([1..1])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an =takerleiN a anyWalk
                             in (an)
       -- drop with first occurance if a ','
          let chgAus= map chr (dropWhile (>44) (map ord (concat auswahl))) 
          
          let tag = let an =  cvsZeilenBreak (concat auswahl)
                    in map chr an
 
          let foprocess = tag ++ ","
 
 
          let process = (concat auswahl) \\ show foprocess
-- neue zeile nach JEDER leerstelle niicht gut geeignet weil
          let geert =  show(unlines(words ( process))) 
          let iterator = let a1 r t = unto (length whereSpaces) r t
                         in let a2 = (a1 writePath2ValFile0 process)
                         in a2 -- ++["\n"] 
         
         -- let exxport = concat a \\ show b
          
          let aparser =  tag ++"\n\n\n"++ process --
   
          let dataTyp =  iterator  -- 

          return (unlines dataTyp) ) 

     let iteraVal = unlines dipfade
                       --  let a1 r t = unto 3 r t --(length whereSpaces) r t
                   -- in let a2 =  (writePath2ValFile1 (concat iterator)  (tag))
                        -- in let a3 = a1
     let bobbo2 = [1..selectLine]
     dipfadSchritte <- forM (bobbo) (\ a -> do
                let getVal = takerleiN selectLine ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                return ((show getDaten)) )
     derInhalt <- forM ([1..1]) (\ a -> do
                let getVal = takerleiN selectLine ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                let getfstNumber = let stp1 = let g1 = cvsZeilenBreak2 ( getDaten)
                                              in map chr g1
                                 --  in let cvsZeilenBreakL 
                                   in stp1
     -- get the chrs of eg " 4.335 z m 2.22345"
                let getChrs fogetData = let stp1 = map ord fogetData
                              in let stp2 = fogetData \\ getfstNumber
                              in let stp3 =  (cvsZeilenBreakL (fogetData))
                              in if stp3 >= [(ord 'A')] then getfstNumber++(map chr stp3)
                                 else (stp2) --map chr stp3)
                let iteraSnd = let a1 e r = unto 1 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( getDaten) --(getChrs getDaten) getDaten
                                in let a3 = (fogetSnd getDaten getfstNumber)
                                in let a4 = a3 --a2 a3
                                in  a4 --  (a2 a3)   
                let iteraChrs = let a1 e r = unto 4 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( iteraSnd) --(getChrs getDaten) getDaten
                                in let foa3 f g = fst ( fogetChrs f g)
                                in let foa4 f g = snd ( fogetChrs f g)
 
                                in let a3 f = (foa3 f getfstNumber)
                                in let a4 f = (foa4 f (a3 f ))
                                in let fstLetter = last( a1 a3 iteraSnd)
 -- the new datastream everyting after fst ltter
                                in let lastLetter = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = iteraSnd\\fowek
                                                in last(a1 ( a3) (wek)) 
                                in let sndVal = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let focontent = (concat(drop 1 (take 2 iterA1))) 
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = (a4 iteraSnd)\\fowek
                                                in let conTend = head (a1 ( a3) (wek)) 
                                                in let tend =  (a4 iteraSnd)\\fowek
                                                in let fertig = tend \\lastLetter
                                                in fertig
                               -- in let a4 = lines (show a3) --a2 a3
                                in  [fstLetter,lastLetter,sndVal] --(a1 ( a4) (iteraSnd))--  (a2 a3)                  
                return ((show getfstNumber )++show iteraSnd++show iteraChrs) )
    -- mapM putStrLn (dipfade)
     putStrLn ( show (length whereSpaces))
    -- putStrLn (show iteraVal)
     putStrLn (unlines dipfadSchritte)
     putStrLn (unlines derInhalt)
     writeFile (root++"textS/testInhalt.txt") (concat derInhalt)
    --
    -- unto
-------------------------------------------------------------------
-- COPY OF ABOVE FUNCTION WITH DIFFERENT OUTPUT
-- WILL BE WRITING JUST 1 LINE 
-------------------------------------------------------------------
-- MainSelecor Function can select Data beteen Kommata within
-- a Path, can select up to max 2 letters within Kommata 
-- file2read            
writePath2ValLine file2read selectLine file2add2= do
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile (root++ file2read) --svgExpert1.txt") --Path2ValDATA/dininSpez1kompl.txt") -- z.b. "mageWalAllPath.txt"--durble.csv
     let leane = length anyWalk
     let whereSpaces = let x1 =  (takerleiN 1 anyWalk)
                       in let x2 = ',' `elemIndices` (concat x1)
                       in x2 
    
     let bobbo =  ([1..1])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an =takerleiN a anyWalk
                             in (an)
       -- drop with first occurance if a ','
          let chgAus= map chr (dropWhile (>44) (map ord (concat auswahl))) 
          
          let tag = let an =  cvsZeilenBreak (concat auswahl)
                    in map chr an
 
          let foprocess = tag ++ ","
 
 
          let process = (concat auswahl) \\ show foprocess
-- neue zeile nach JEDER leerstelle niicht gut geeignet weil
          let geert =  show(unlines(words ( process))) 
          let iterator = let a1 r t = unto (length whereSpaces) r t
                         in let a2 = (a1 writePath2ValFile0 process)
                         in a2 -- ++["\n"] 
         
         -- let exxport = concat a \\ show b
          
          let aparser =  tag ++"\n\n\n"++ process --
   
          let dataTyp =  iterator  -- 

          return (unlines dataTyp) ) 

     let iteraVal = unlines dipfade
                       --  let a1 r t = unto 3 r t --(length whereSpaces) r t
                   -- in let a2 =  (writePath2ValFile1 (concat iterator)  (tag))
                        -- in let a3 = a1
     let bobbo2 = [1..1]
     dipfadSchritte <- forM (bobbo2) (\ a -> do
                let getVal = takerleiN selectLine ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                return ((show getDaten)) )
     derInhalt <- forM ([1..1]) (\ a -> do
                let getVal = takerleiN selectLine ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                let getfstNumber = let stp1 = let g1 = cvsZeilenBreak2 ( getDaten)
                                              in map chr g1
                                 --  in let cvsZeilenBreakL 
                                   in stp1
     -- get the chrs of eg " 4.335 z m 2.22345"
                let getChrs fogetData = let stp1 = map ord fogetData
                              in let stp2 = fogetData \\ getfstNumber
                              in let stp3 =  (cvsZeilenBreakL (fogetData))
                              in if stp3 >= [(ord 'A')] then getfstNumber++(map chr stp3)
                                 else (stp2) --map chr stp3)
                let iteraSnd = let a1 e r = unto 1 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( getDaten) --(getChrs getDaten) getDaten
                                in let a3 = (fogetSnd getDaten getfstNumber)
                                in let a4 = a3 --a2 a3
                                in  a4 --  (a2 a3)   
                let iteraChrs = let a1 e r = unto 4 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( iteraSnd) --(getChrs getDaten) getDaten
                                in let foa3 f g = fst ( fogetChrs f g)
                                in let foa4 f g = snd ( fogetChrs f g)
 
                                in let a3 f = (foa3 f getfstNumber)
                                in let a4 f = (foa4 f (a3 f ))
                                in let fstLetter = last( a1 a3 iteraSnd)
 -- the new datastream everyting after fst ltter
                                in let lastLetter = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = iteraSnd\\fowek
                                                in last(a1 ( a3) (wek)) 
                                in let sndVal = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let focontent = (concat(drop 1 (take 2 iterA1))) 
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = (a4 iteraSnd)\\fowek
                                                in let conTend = head (a1 ( a3) (wek)) 
                                                in let tend =  (a4 iteraSnd)\\fowek
                                                in let fertig = tend \\lastLetter
                                                in fertig
                               -- in let a4 = lines (show a3) --a2 a3
                                in  [fstLetter,lastLetter,sndVal] --(a1 ( a4) (iteraSnd))--  (a2 a3)                  
                return ((show getfstNumber )++show iteraSnd++show iteraChrs) )
    -- mapM putStrLn (dipfade)
    -- putStrLn ( show (length whereSpaces))
    -- putStrLn (show iteraVal)
    -- putStrLn (unlines dipfadSchritte)
  --   putStrLn (unlines derInhalt)
     putStrLn "if-Error: might need to WRITE EMPTY FILE first"  
     add [(root++ file2add2),(concat derInhalt)]
------------------------------------------------------------------
--textS
writePath file2read rt file2add2= do 
             let se1 = [1..(rt)]
             amap <- forM se1 (\as -> do  
                    let zu = writePath2ValLine file2read as file2add2
                    do zu 
                    return (zu))
             putStrLn "added to svgExpert2.txt" 
           
-------------------------------------------------------------------
-- STEP 2
-- IMPORT simulated into a patternfile (val,day,month,year)
-- so that the runKAGO Simulation from module DreiBandTEST 
-- can be run in below aKA function
-- find function  at bottom of file
--val2PatternFile ("aaKA/aaKAnsatz"++token++".txt") "senior.txt"
-------------------------------------------------------------------
-------------------------------------------------------------------
--STEP 3
--RUN SIMULATION 
-- f:string e.g"1" how many steps -> one step simulates 10 vals
-- randm:string e.g"100"
-- gebraucht in exportStream
aKA f randm= putStrLn "SET TOREST" --(runKAGO f randm  )
-------------------------------------------------------------------

-------------------------------------------------------------------
-- STEP 1
-- MainDataStream Function can select Data beteen Kommata within
-- a Path, can select up to max 2 letters within Kommata 
-- selectData : 
-- exportStream 1 --export fstVal
--     "        2 -- "     sndVal
--     "        3 -- "     fstLetter
--     "        4 -- "     sndLetter
--     "        >=  5 -- exports all Data of one Line
--token: which aaKAnsatz file (simulated vals )to read
--token:String e.g "1"
--forrandom:String e.g "100" every number will generate an new simulation
exportStream selectData g = exportStreamRaw  (sourceAlWall) selectData ("textS/aaKAnsatz"++g++".txt")   

--dininSource: the allWallPath File could be dininSpez1kompl
exportStreamRaw dininSource selectData g = do --tokenN = do
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile dininSource -- z.b. "mageWalAllPath.txt"--durble.csv
     let leane = length anyWalk
     let whereSpaces = let x1 =  (takerleiN 1 anyWalk)
                       in let x2 = ',' `elemIndices` (concat x1)
                       in x2 
   
     let bobbo =  ([1..1])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an =takerleiN a anyWalk
                             in (an)
       -- drop with first occurance if a ','
          let chgAus= map chr (dropWhile (>44) (map ord (concat auswahl)))
 
         --will take everything BEFORE the first kommata
          let tag = let an =  cvsZeilenBreak (concat auswahl)
                    in map chr an
 
          let foprocess = tag ++ ","
 
 
          let process = (concat auswahl) \\ show foprocess
          let iterator = let a1 r t = unto (length whereSpaces) r t
                         in let a2 = (a1 writePath2ValFile0 process)
                         in a2 -- ++["\n"] 
         
         -- let exxport = concat a \\ show b
          
          let aparser =  tag ++"\n\n\n"++ process --
   
          let dataTyp =  iterator  -- 

          return (unlines dataTyp) ) 

     let iteraVal = unlines dipfade
                       --  let a1 r t = unto 3 r t --(length whereSpaces) r t
                   -- in let a2 =  (writePath2ValFile1 (concat iterator)  (tag))
                        -- in let a3 = a1
     let bobbo2 = [1..1]
-- mapt die einzelne Zeile (writePath2ValFile1)-funktion mit einer iteration 
-- in der monade, exportiert   
     dipfadSchritte <- forM (bobbo) (\ a -> do
                let getVal = takerleiN 1 ( iteraVal)
                --will take everything BEFORE the first kommata
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                return ((show getDaten)) )
     let solang = [1..(length whereSpaces)]
-- exportiert 1
--            2
--            3
--            4
-- selector fuer main 
    -- dataStream <-  forM (solang) (\ a -> do 
     derInhalt <- forM (solang) (\ a -> do
                let getVal = takerleiN  a ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                let getfstNumber = let stp1 = let g1 = cvsZeilenBreak2 ( getDaten)
                                              in map chr g1
                                 --  in let cvsZeilenBreakL 
                                   in stp1
                let getChrs fogetData = let stp1 = map ord fogetData
                              in let stp2 = fogetData \\ getfstNumber
                              in let stp3 =  (cvsZeilenBreakL (fogetData))
                              in if stp3 >= [(ord 'A')] then getfstNumber++(map chr stp3)
                                 else (stp2) --map chr stp3)
                let iteraSnd = let a1 e r = unto 1 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( getDaten) --(getChrs getDaten) getDaten
                                in let a3 = (fogetSnd getDaten getfstNumber)
                                in let a4 = a3 --a2 a3
                                in  a4 --  (a2 a3)   
                let iteraChrs = let a1 e r = unto 4 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( iteraSnd) --(getChrs getDaten) getDaten
                                in let foa3 f g = fst ( fogetChrs f g)
                                in let foa4 f g = snd ( fogetChrs f g)
 
                                in let a3 f = (foa3 f getfstNumber)
                                in let a4 f = (foa4 f (a3 f ))
                                in let fstLetter = last( a1 a3 iteraSnd)
 -- the new datastream everyting after fst ltter
                                in let lastLetter = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = iteraSnd\\fowek
                                                in let expo = last(a1 ( a3) (wek)) 
                                                in let goexpo  = if (length expo) == 0 then [] 
                                                                 else ((a) : [] )
                                                in (  expo)
                                in let sndVal = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let focontent = (concat(drop 1 (take 2 iterA1))) 
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = (a4 iteraSnd)\\fowek
                                                in let conTend = head (a1 ( a3) (wek)) 
                                                in let tend =  (a4 iteraSnd)\\fowek
                                                in let fertig = tend \\lastLetter
                                                in fertig
                               -- in let a4 = lines (show a3) --a2 a3

                                in  [fstLetter,lastLetter,sndVal] -- if selectData == 1 then [getfstNumber]
                                  -- else if selectData == 2 then [sndVal]
                                  -- else if selectData == 3 then [fstLetter]
                                  -- else if selectData == 4 then [lastLetter]
                                  -- else [fstLetter,lastLetter,sndVal] --(a1 ( a4) (iteraSnd))--  (a2 a3)
                               
                let sltData = if selectData == 1 then  getfstNumber
                                   else if selectData == 2 then  ((last iteraChrs)) -- getsndVal
                                   else if selectData == 3 then show ( (take 1 iteraChrs))
                                   else if selectData == 4 then show (head (drop 1 (take 2 iteraChrs)))
                                   else  ((show getfstNumber )++show iteraSnd++show iteraChrs) --                 
                return( sltData) ) -- ((show getfstNumber )++show iteraSnd++show iteraChrs) ) 

-- lies die Simulation 
-- token: e.g "6" -> Simulation 
-- kombinier mit simulierten Stream 
-- bau in xhtml/SVG format
--solang 
     fpfad <- readFile (root++ g ) --("aaKA/aaKAnsatz"++token++".txt")
     let slangRoot = length (lines fpfad)
     neuerPfad <- forM ([1..slangRoot]) (\ a -> do
    -- neuerPfad <- forM ([1..80]) (\ a -> do
            
                let simu = let taKe = takerleiN  (a-1) (fpfad )
                           in if a == 1 then [""]
                              else taKe
                let getVal = takerleiN  a ( iteraVal)
                let tag2 h = let an =  cvsZeilenBreak (concat h)
                             in map chr an
     -- gets the first of the iterative list somthing 
     -- in accordance to break criteria ',' 
                let getDaten = writePath2ValFile1  (concat getVal) (tag2 ( getVal)) 
                let getfstNumber = let stp1 = let g1 = cvsZeilenBreak2 ( getDaten)
                                              in map chr g1
                                 --  in let cvsZeilenBreakL 
                                   in stp1
                let getChrs fogetData = let stp1 = map ord fogetData
                              in let stp2 = fogetData \\ getfstNumber
                              in let stp3 =  (cvsZeilenBreakL (fogetData))
                              in if stp3 >= [(ord 'A')] then getfstNumber++(map chr stp3)
                                 else (stp2) --map chr stp3)
                let iteraSnd = let a1 e r = unto 1 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( getDaten) --(getChrs getDaten) getDaten
                                in let a3 = (fogetSnd getDaten getfstNumber)
                                in let a4 = a3 --a2 a3
                                in  a4 --  (a2 a3)   
                let iteraChrs = let a1 e r = unto 4 e r
                               -- in let foa2 t z = t \\ z
                                in let a2 e = a1 e ( iteraSnd) --(getChrs getDaten) getDaten
                                in let foa3 f g = fst ( fogetChrs f g)
                                in let foa4 f g = snd ( fogetChrs f g)
 
                                in let a3 f = (foa3 f getfstNumber)
                                in let a4 f = (foa4 f (a3 f ))
                                in let fstLetter = last( a1 a3 iteraSnd)
 -- the new datastream everyting after fst ltter
                                in let lastLetter = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = iteraSnd\\fowek
                                                in let expo = last(a1 ( a3) (wek)) 
                                                in let goexpo  = if (length expo) == 0 then [] 
                                                                 else ((a) : [] )
                                                in (  expo)
                                in let sndVal = let iterA1 = a1 a3 iteraSnd 
                                                in let ste1 t = (t++"  ")
                                                in let focontent = (concat(drop 1 (take 2 iterA1))) 
                                                in let fowek = ste1 (concat(drop 1 (take 2 iterA1)))
                                                in let wek = (a4 iteraSnd)\\fowek
                                                in let conTend = head (a1 ( a3) (wek)) 
                                                in let tend =  (a4 iteraSnd)\\fowek
                                                in let fertig = tend \\lastLetter
                                                in fertig
                               -- in let a4 = lines (show a3) --a2 a3
                               
                                -- wheet out e 's from processing added 25-08-19
                                in let simuFilter = lines (replaceE (unlines simu))
                                                                                    
                                in  [( simuFilter++(lines iteraSnd))] -- if selectData == 1 then [getfstNumber]




                                  -- else if selectData == 2 then [sndVal]
                                  -- else if selectData == 3 then [fstLetter]
                                  -- else if selectData == 4 then [lastLetter]
                                  -- else [fstLetter,lastLetter,sndVal] --(a1 ( a4) (iteraSnd))--  (a2 a3)
                               
                let sltData = let a1 b = drop (b-1) ( take b iteraChrs)
                             -- ( in let a2 = if (length whereSpace)==e
                              in iteraChrs -- (take (length whereSpaces) (map a1 [1,3..])))

                     --        if selectData ==  then  getfstNumber
                       --            else if selectData == 2 then  ((last iteraChrs)) -- getsndVal
                         --          else if selectData == 3 then show ( (take 1 iteraChrs))
                           --        else if selectData == 4 then show (head (drop 1 (take 2 iteraChrs)))
                             --      else  ((show getfstNumber )++show iteraSnd++show iteraChrs) --                 
                return(concat (concat sltData)) ) -- ((show getfstNumber )++show iteraSnd++show iteraChrs) )
        -- will just insert the lines of a simlation that could not have been
                                -- simulated, can only simulate steps of ten
     let finalFileFix = let orgiLength = length whereSpaces 
                        in let simULength = slangRoot `div` 2
                        in let calcDif = (orgiLength) - slangRoot --( simULength)
                        in let calcTaker = orgiLength - calcDif
                       -- take from Source get the last calcDif number as last lines of source
                       -- in order to substitute the lines not simulated
                        in let fromSource = ((take 1 ((lines anyWalk)))) -- take Source File
                        in let outLength = length (unlines fromSource)
                        in let moreSource =  show ( ( reverse (take (calcDif) (reverse whereSpaces)))) --(take 6 (unlines fromSource))
                        in let readSource = drop (head(read moreSource)) (take (last(read moreSource)) (unlines fromSource)) --reverse (take (outLength - (head(read moreSource))) (reverse (unlines fromSource)))
                        in let prepMo = (length ((words readSource)))*2
                        in let readSource2 = filter (/=',') (readSource)
                        in let porcSource r = unwords (drop ((r-1)) (take (r) ((words readSource2))))
                        in let porcSource2 r = unwords (drop ((r-1)) (take (r) ((lines readSource2))))
                -- test---------
                        in let readSource33 = drop ((head(read moreSource))+1) (take ((last(read moreSource))+1) (unlines fromSource)) 
                        in let porcSource3 r = unwords (drop ((r-1)) (take (r) ((words readSource33))))

                      --  in let forListSource = ceiling (calcDif `div` 2)
                      --  maps the unsimulated fst and snd of a line e.g: -0.456 3455 -> fst snd
                        in let getfstfLine h h2 = (unwords (  (map porcSource3 [h]))) ++" "++  (fst (break (<=',') (unwords   (map porcSource3 [h2])))) -- fstLine
                        in let getsndfLine2 h2 h3 = (filter (/=',') (snd (break (<=',') (unwords (  (map porcSource3 [h2])))))) ++" "++  (fst (break (<=',') (unwords   (map porcSource3 [h3]))))
                        in let makeSubstituteLine = let ste1 h h2= getfstfLine h h2
                                                    in let ste2 h2 h3 = getsndfLine2 h2 h3
                                                    in let both h h2 h3 = (getfstfLine h h2) ++ (getsndfLine2 h2 h3)
                                                    in let both2 he = (both (he-1) (he) (he+1)) 
                                                    in let someLijst dat = take dat [1,4..]
                                              -- calcDif-1 ( due to drop Last line will be inserted in SVG builder lateron)
                                              --
                                                    in let checkLengthUnsim = (someLijst (calcDif-1))
                                                    in let iffer h = if (even h)==True then  (getsndfLine2 (h) (h+1))
                                                                   else (getfstfLine (h) (h+1))
                                                    in unlines (map iffer [1..(calcDif-1)])
                                                      -- else "" 
                        in makeSubstituteLine -- readSource  

  -- fuege simulierten Stream in den Wx.Maxima file Writer ein 
     -- > write "senior.txt"
     let foMAx = outPutMaximafunct ( derInhalt) [1.. ((slangRoot))] 
     let writeWXFile2 = (aCompleteWX ( [(foMAx)]) [[1,2,3],[4,5,6]] "20" "0.49" "0.60")
     -- IMPORT simulated into a patternfile (val,day,month,year 
   --  val2PatternFile ("aaKA/aaKAnsatz"++token++".txt") "senior.txt"
    -- let aKA = (runKAGO3 "10" "100" "senior.txt" "blum.txt" )
--     let aKA2 = fobundleaM22 2 "20" "100"

    -- aKA 
    -- print aKA
  --   aKA2 
    -- mapM putStrLn (dipfade)
  {-   putStrLn ( show (length whereSpaces))
    -- putStrLn (show iteraVal)
     putStrLn (unlines dipfadSchritte)
     putStrLn ((unlines ( (derInhalt))))
     writeFile "testInhalt2.wxm" ((""++ writeWXFile2))
     putStrLn (show ( foMAx)) -}
     let expoWithoutfstLine = let as1 = length derInhalt
                             in drop 1 ( take as1 derInhalt)  
     writeFile (root++"textS/testInhalt.txt") (unlines (expoWithoutfstLine))--(derInhalt))
     
     (print(((unlines ( (neuerPfad))))))
     let exppath = let a1 f = unlines f
                   in ( ""++(a1 neuerPfad))
     let gzui = mapM putStrLn (neuerPfad)
     gzui
     writeFile (root++"textS/testInhalt3.txt") ((unlines (neuerPfad))++ (finalFileFix))
     writeFile (root++"textS/testSimu.txt") (( (fpfad)))

     putStrLn exppath 
     putStrLn ("\n check final file"++(show(length whereSpaces)++(show slangRoot)))
     --(print([ foMAx]))
    -- putStrLn  --SVGtxt
-----------------------------------------------------------------
-----------------------------------------------------------------
-- file2read: String
-- token e.g: "1" which of the files in folder aaKA that exportStream s.a.
--                exported to read
-- writes SVG File
--                 
checkfad file2read token= do
     asd <- readFile (root++file2read)
     let asd2 = let a1 = lines(concat [asd])
                in let a2 = lines asd
                in a2
-- determines length of the simulated Vals file that is the newest file of the link below
-- the file was generated by 
      
  --   thisF <- readFile ("aaKA/aaKAnsatz"++token++".txt")
   --  let longs = length (lines thisF)
         
     
     let checker = (length (lines asd))
     let cheKK = [1..checker] 
     let getRitBetter = map chr(( cvsZeilenBreak32 (unlines [asd])))
-- wandel simu stream in xhtml line
     filTer <- forM ( cheKK ) (\ a -> do 
            let auswahl = takerleiN a asd
            let getRit =  map ord (unlines auswahl) --(cvsZeilenBreak32 (unlines auswahl))
            let tweakOrd = let a1 v d = v : d  -- v wird eingefuegt
                           in let foa21 = reverse getRit
                           in let a2 v = a1 v foa21
                           in let komma = 44 -- ord ',' 
                               -- in fst line make "0.0 c 3.456" -> "c 3.456" , so that
                               -- checkpdad can offer Coordinates via path4SVG or path4SVGControl 
                           in if a==1 then unwords(tail(words(map chr (reverse (a2 komma)) )))
                              else map chr (reverse (a2 komma))  
            return (lines tweakOrd)) --((((map chr getRit))++",") )   )
            
     putStrLn (show asd2)
     print ( filTer)
     writeFile (root++"textS/testInhalt4.txt") (show((unwords(concat filTer)))) 
    
     print (path4SVG(show((unwords(concat filTer))))) 
     writeFile (root++"textS/testInhaltSVG.txt") (unlines(concat(map lines(path4SVG(((concat(concat filTer))))))))
     let fgt = (unlines(concat(map lines(path4SVG(((concat(concat filTer))))))))  
     -- write SVG File
     makeSVGFile fgt
-----------------------------------------------------------------
-----------------------------------------------------------------
makeSVGFile b  = do

    headerR <- readFile (root++"textS/TheSVGHeader.txt")
    onesvg <- readFile  (root++"textS/dininWalkModule.txt")
    mof <- readFile (root++ "textS/motionFunction.txt")
    mof2 <- readFile (root++ "textS/motionFunction2.txt")
    mof3 <- readFile (root++ "textS/motionFunction3.txt")
    mof4 <- readFile (root++ "textS/motionFunction4.txt")
   -- hobgob2 <- readFile "hobgobAttNoWaponPaths.txt"
    hobgob1 <- readFile (root++ "textS/tete.txt")
    
    let insertDifade = b
  --  add ["toxic.svg", svghead]
    --
   -- mapM putStrLn [colors]  
    let endSVGtag = ("</svg>")
 --   let endHTmltag = (endSVGtag ++"\n  </body>\n </html>")
--    add ["toxic.svg",endHTmltag] 
 --   writeFile "toxi.svg" bert
 --

  --  let bert = (headerR ++ onesvg ++ mof ++ onesvg ++ mof2 ++ hobgob1 ++mof3++ insertDifade++"</g> "++ endSVGtag)
    let bert = (headerR ++ mof3 ++ insertDifade++ endSVGtag)

    writeFile (root++"toxi.svg") bert
-----------------------------------------------------------------
-----------------------------------------------------------------
-- Xhtml/SVG format "<g> defines graphic path.. </g>" template
-- path: String
-- wird in in MakeSVGFile eingesetzt  
path4SVG  path = 
 ( ["<g >\n"++ -- transform=\"translate(0,55)\">\n"++ -- visibility=\"hidden\">\n"++
 "      <path\n"++
 "        style=\"fill:#333338;fill-opacity:1;stroke:#1c1c49;stroke-width:0.11658414;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1;visibility:hidden\"\n"++
 "          d=\"m 200 195 " ++ (unlines [path])++" 0 z\" id=\"25\" >       <set attributeName=\"visibility\" attributeType=\"CSS\" to=\"visible\" begin=\"5.799999999999999s\" dur=\"0.2s\" fill=\"freeze\"/>\n"++
-- "      <set attributeName=\"visibility\" attributeType=\"CSS\" to=\"hidden\" begin=\"5.999999999999999s\" dur=\"0.4s\" fill=\"freeze\"/>\n"++
 "      </path> </g>\n"])
--same as above but need Coordinates x , Y
path4SVGControl foxxX foyyY path = 
 ( ["<g >\n"++ -- transform=\"translate(0,55)\">\n"++ -- visibility=\"hidden\">\n"++
 "      <path\n"++
 "        style=\"fill:#333338;fill-opacity:1;stroke:#1c1c49;stroke-width:0.11658414;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1;visibility:hidden\"\n"++
 "          d=\"m "++foxxX++" "++foyyY++" " ++ (unlines [path])++" 0 z\" id=\"25\" >       <set attributeName=\"visibility\" attributeType=\"CSS\" to=\"visible\" begin=\"5.799999999999999s\" dur=\"0.2s\" fill=\"freeze\"/>\n"++
-- "      <set attributeName=\"visibility\" attributeType=\"CSS\" to=\"hidden\" begin=\"5.999999999999999s\" dur=\"0.4s\" fill=\"freeze\"/>\n"++
 "      </path> </g>\n"])

--------------------------------------------------------------------


-----------------------------------------------------------------
-----------------------------------------------------------------
    -- quelle simulierte vals
    --  laenge val liste 
    --  wird in exportStream gebraucht um WXmaaxima file zu generieren
outPutMaximafunct goghi obo =
                     let a =  (goghi) -- quelle simulierte vals
                         b = (map show obo) -- laenge val liste 
                       --  c = concat (concat dipfade4)
                         kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                         fofiltern x y= show (kerry x y)
                         filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                            tss2 = filter (/=34) tussenstap
                                        in map chr (tss2)
                      -- ( simuliiert , Quelle)
                      in  (filtern b a)

-----------------------------------------------------------------
-----------------------------------------------------------------  
-- functions aids to shorten code 
    -- x:Int ; n:[Int]
takenN x n = concat (drop  (x-1) ( take x n))


-----------------------------------------------------------------
-----------------------------------------------------------------
--list for accesFuncWX below
-- exportiert:  [(filtern b a),(filtern b c)]
-- die (simulierten Vals, einen zweiten stream )
-- DEFINE WX MAXIMA kann viele streams in einem
-- Koordinatensystem plotten 
-- wird in 
outPutMaxima3 goghi dipfa bob = 
                let a =  (concat goghi) -- quelle simulierte vals
                    b = (map show bob) -- laenge val liste 
                    c = (concat dipfa)
                    kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                    fofiltern x y= show (kerry x y)
                    filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                       tss2 = filter (/=34) tussenstap
                                   in map chr (tss2)
                in [(filtern b a),(filtern b c)]
-----------------------------------------------------------------
-----------------------------------------------------------------
--- Extraordinary function chooses which AND howmany functions of the list above will be put in output
-- l: [Int] ~ oder auch welche kombination von funktionen
-- bob laenge val liste
-- ghogi quelle1 simukieter val
-- dipfa quelle2 
accesFuncWX l goghi dipfa bob =
                  let aw1 n = (takenN n (outPutMaxima3 goghi dipfa bob))
                  in let wielanGg  = [1..l]
                  in let aw2 = map aw1 wielanGg
                  in let foaw1 = show bob
                  in let enExp a b sqale1 sqale2 = (aCompleteWX a b foaw1 sqale1 sqale2) -- diese display nach compiliren  vs aCompleteWX2 schreibt display in file     
                     --  in let aw3 =  ceiling (l/2)	
                  in let aw4 = ([wielanGg ,[(l+1)..(l*2) ]])
                  in let aw5 = "0.0" 
                  in let aw6 = "100"              
                  in enExp aw2 aw4 aw5 aw6 --enExp            
-------------------------------------------------------------
-------------------------------------------------------------
--simulierter Stream in exportStream wird in
-- Patternfile -- PATTERN:  val,year,monat,tag 
-- eingesetzt -> laedt "seto.txt" und fuegt
-- neue simulierte Vals ein 
--source File: x
--zu bearbeitende File: nf
--starte Zeile: anfang (Int)
--ende X Zeilen spaeter: xX Int 
-- hat mMonade die string angibt der fuer exportStream
-- durch dieses verfahren kann das modul DreiBandTEST : runKAGo 
-- in der funktion aKA s.o. verwendet werden- welches der
--  eigentliche Simulator ist
val2PatternFile xin nFin = do
     putStrLn "set to read from textS folder"
     let x= (root++"textS/"++xin)
     let nF = (root++"textS/"++nFin) 
--Step4
--Acces 4 readymade Pattern files
--Zeilenbreak criterium fuer CVS datei  
--access to date: day   
     let cvsZeilenBrea  s =
                      let a = map ord s  --ord '-'=45
                      in let teile = takeWhile (/=44) a
                      in teile

--acces to val
     let cvsZeilenBrea2  s =
                      let a = map ord (s)  --ord '-'=45
                      in let teile = break (>=57) a
                      in fst teile

--acces to val
     let cvsZeilenBrea3  s =
                      let a = map ord (s)  --ord '-'=45
                      in let teile = break (>=57) a
                      in snd teile

     let cvsZeilenBrea4  s =
                      let a = map ord (s)  --ord '-'=45
                      in let teile = dropWhile (>45) a
                      in teile

     putStrLn$ "Enter starting Line:"
     anfang <- getLine
     putStrLn$ "How many Lines to change?:"
     xX <- getLine
     
   --  database <- readFile "milch.txt"
     --let machAdd f = add [ nF, f]
     anyWalk <- readFile x -- z.b. "aaKA/aakaWX8.txt" real vals
     getKalender <- readFile (root++"textS/seto.txt")
   --  machAdd dataTyp 
     --let bilderno = length anyWalk  
     let bobbo =  ([(read anfang)..(read xX)])
     dipfade <- forM (bobbo) (\ a -> do
          let auswahl      = let an =takerleiN a anyWalk
                             in an

          let val = let an =  cvsZeilenBrea (concat auswahl)
                    in let boss = filter (>40) an 
                    in map chr boss
 
          let auswahlK      = let an =takerleiN a getKalender
                              in an
          let valK = let an =  cvsZeilenBrea (concat auswahlK)
                    in let boss = filter (>40) an 
                    in map chr boss
          let process = (concat auswahlK) \\ show valK
 
          let findYear =  let an =  (cvsZeilenBrea2 (process))
                          in map chr an 

          let year  = let an = show findYear
                      in let b = map ord an
                      in let c = filter (>45) b --schmeisst kommas raus
                      in let d = filter (<65) c -- laesst nur Zahlen ueber 
                      in map chr c -- bleiben nur Jahreszahlen

          let finddate =  let an =  (cvsZeilenBrea3 (process))
                          in map chr an
     
          let monat = let an = show finddate
                      in let b = map ord an
                      in let c = filter (>45) b --
                      in let d = filter (>64) c -- laesst nur Buchstaben ueber 
                      in map chr d -- bleiben nur Jahreszahlen
             
 
         
          let tag = let an = show finddate
                    in let b = map ord an
                    in let c = filter (>45) b
                    in let d = filter (<65) c 
                    in map chr d 
          let mo3 = show(unlines(words finddate))

          let dataTyp =  ""++"("++val++","++year++","++monat++","++tag++")\n"
          return (dataTyp) ) --a also nur zahlen
     putStrLn " the data is (ohne die extra Leerzeile) : "
     mapM putStrLn (dipfade)
     writeFile nF (concat dipfade)
--------------------------------------------------------------
--------------------------------------------------------------  +++++++++++++++++++++++++++++++++++++++++++++++++++++
--
--avanti
-- source for rndlist 
zufallsBasic t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]

rndLength = zufallsBasic 3 1000000000 10
easyfoNowRun wieviele = zufallsBasic wieviele 5000 10
-- rndList:[String] e.g ["1","55","4","34566543"] any N number which is Int
----------------------------------------------------------------
----------------------------------------------------------------

----------------------------------------------------------------
stringToDouble :: [String] -> [Double]
stringToDouble [] = []
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs


aMaxMinLists grouP = let ste1 = head (take 1 grouP)
                     in let ste2 = if ((show ste1) > (show 0))== True then (maximum grouP )
                                   else minimum grouP       
                     in ste2            
                            --                      else do Just (minimum grouP)
                                               --   in ste1 

closestTo e l = let e#l=snd$minimum$(zip=<<map(abs.(e-)))l 
                 in e#l



-- die gefilterte zahl aus val
foRechne g1 g1L = fog3
 where 
 fog1 = (10^g1L);
 fog3 = (g1/fog1) --(zipWith (*) [(g2)] [addieren])
data FoDoub = FoDoub {value::Double}
instance Show FoDoub where
  show s = show (value s)
  
data Punkt = Punkt {name::String, mother::Maybe Punkt, father::Maybe Punkt,mother2::Maybe Punkt,father2:: Maybe Punkt,middle::Double}
-- Punkt "what" 

-- we show sheep by name
instance Show Punkt where
  show s = show (name s);
  show s = show (middle s)
	

-- traceFamily is a generic function to find an ancestor
--richtungHoeheY :: Punkt -> [ (Punkt -> Maybe Punkt)] `&&`  [(Punkt -> Maybe Double) ] -> Maybe Punkt
richtungHoeheY s l = foldM getParent s l
  where getParent s f = f s

-- we can define complex queries using traceFamily in an easy, clear way
--BEISPIEL:
--paternalGrandmother        ssnd (break (==46) ab)snd (break (==46) ab) = traceFamily s [father, mother]
--mothersPaternalGrandfather s = traceFamily s [mother, father, father]

-- AN DIESERR STELLE KAN EIN ANDERES SYSTEM ZUR 
-- BESCHREIBUNG DES GEWAEHLTEN PFADES EINGSETZT WEREDEN
-- ZUR vEREINFACHUNG WIRD ERST ALLES WIE IM BEISPIEL DataReihen.hs
-- PROGRAMMMIERT

logikPfadinYRichtung        s = richtungHoeheY s [mother, father] --[geradeaus, rechts]
mothersPaternalGrandfather s = richtungHoeheY s [mother, father, father]
--
-- 1.DIE OBIGEN pFAD SUCHFUNKTIONEN
-- 2.DIE STAMBAUMFUNKTION (INTERGRIERTER BINAEBAUM)
--
--
--
--DEM SET KOMMT EINE BESONDERE BEDEUUTNG ZU DA
--ES IN EINEM ES EINE QUELLE IN SICH SELBST IST 
--(INJECTIV 
--UND  
--
--
see s = richtungHoeheY s [father,father]
-- this allows the user to name the mother and father functions on the command line
-- kann hier nichteuklidsche logik programmiert werden?
getFunctionByName :: String -> (Punkt -> Maybe Punkt)
getFunctionByName "father" = father
getFunctionByName "mother" = mother
getFunctionByName "mother2" = mother2
--getFunctionByName "middle" =  middle
getFunctionByName _        = error "Invalid function name - not 'mother' or 'father'"

--getDoublle :: String -> (Punkt -> Maybe String)
--getDoublle "middle" = middle

bogus = "Greenthump"
-- this builds our sheep family tree
--zweierDiagram :: Punkt

--zweierDiagram  = let twos = Punkt "Gorden" Nothing Nothing Nothing Nothing Nothing
  ----               in let three = Punkt "red" Nothing Nothing Nothing Nothing Nothing               
      --           in let two = Punkt "father"   (Just twos) Nothing Nothing Nothing Nothing       
        --         in two
        --


-- Reads optimizer..txt file filters:
-- zeiLLe:Int welche zeile
-- token:String for reading file
-- choose:Int 1-->max 2->min 3->snd biggest 4->snd smallest
--            5-->RealVal 6..8->Punkt String .. .. .. .. double
-- above are possible search criteria
-- writeE:Int if 1 then write new WX
--            if 2 write NEW File aaKA/aakaLRun++NEW ++txt
--                 always writes writeE+1; incremented
--                 by one
--            if 3 then add a choose see above to aakaLRun++(writeE( x+1))++txt
--            else then just show
-- writeToKA:String e.g  "aakaWX" decide which to write aaKAnsatz or aakaWX or aakaLRun
---------------------------------------------------------------------------------------
-- token2:String e.g "2" 
-- simuWrite:Int for writing Simulation file ( ("aaKA/aakaWX")) 
searchLineLogic zeiLLe token choos writeE = (searchLineLogicRaw ("aaKA/aakaWX") zeiLLe token "8" choos writeE ("2") ("aaKAnsatz")) 
--token2:String Empy string or e.g"1" which aakaWX lesen
searchLineLogicRaw readIn2 zeiLLe token token2 choos writeE simuWrite writeToKA= do 
    the10SimuS <- readFile (root++"optimizer"++token++".txt")
    -- zum plotten
    theRealS <- readFile (root++readIn2++(token2)++".txt") -- und als vergleich zur simulation
  -- length of a line of "optimizer"++token++".txt")
  -- defines how many steps in foSolang
    let foSolang = [1]
    optimLine <- forM (foSolang) (\ a -> do
             -- get one line of optimizer..txt
             let auswahlIn t =  (drop (t-1) (take t (lines the10SimuS)))
             let ridString = let ins1 = map ord (unlines (auswahlIn zeiLLe))
                             in let ins2 = filter (>34) ins1 -- filter alle ' [ ' und '] '
                   -- in let ins3 = filter (>=32) ins2
                             in (map chr ins2)

             let whereSpaces  = let x1 =  (auswahlIn zeiLLe)
                       in let x2 = ',' `elemIndices` (concat x1)
                       in x2 

             let tag = let an =  cvsZeilenBreak (ridString)
                       in map chr an
 
             let foprocess = tag ++ ","
 
 
             let process = (concat (auswahlIn zeiLLe)) \\ show foprocess
             let iterator = let a1 r t = unto (length whereSpaces) r t
                            in let a2 = (a1 writePath2ValFile0 process)
                            in a2 -- ++["\n"] 
             let export  = (foprocess++(unwords iterator)) --unlines (auswahlIn t)
             let getEveryVal t = (drop (t-1) (take t (words export)))
             --let filtKlam t  = map chr(cvsZeilenBreak (unlines(getEveryVal t)))
             let filtKlam t = map chr (filter (/=34) (map ord(concat(getEveryVal t))))
             let filtKlam2 t  = map chr(cvsZeilenBreak ((filtKlam t)))
          --   let backOrd t = map ord (filtKlam2 t) 
             
             let turnToStrangeString t = (  [(filtKlam2 t)])
             let avoidOptimcrash = [1..((read token)-2)]
             let machNeun = map turnToStrangeString avoidOptimcrash
             let applyCgraph = (machNeun)
             let op2 m = let op1  = (maximum applyCgraph) -- maximum of String works
                         in let op11 = (minimum applyCgraph)
                         in let ste1 = (applyCgraph \\ [(op1)]) -- 2. groesster val
                         in let ste11 = (applyCgraph \\ [(op11)]) -- 2. kleinster
                         in if (m==1) then ste1
                            else ste11
             let datenTyp e = middle (Punkt "Simu1" Nothing Nothing Nothing Nothing (head e))
             let foReals = stringToDouble (drop (zeiLLe-1) ( take zeiLLe (lines theRealS))) 
             let outPuts = if choos == 1 then [(maximum applyCgraph)]
                           else if choos == 2 then [(minimum applyCgraph)]
                           else if choos == 3 then (op2 1) -- get snd biggest
                           else if choos == 4 then (op2 2) -- get snd smallest
                           else if choos == 5 then [[(show  [foReals])]] -- get snd smallest
                           else if choos == 6 then let nimm1 v= (drop (v-1) (take v applyCgraph))
                                                   in let mediAn = (length avoidOptimcrash) `div` 2
                                                   in nimm1 mediAn
                           else [[(show  [foReals])]]
             return ( show outPuts))
   
       -- nubbing changes the propability of optimizer..txt
       -- gereade wenn mit einem Median gearbeited wird
       -- ist dies wichtig !!!! 
    let fofoOp  = ((concat( optimLine))) 
          -- choose:Int 1-->max 2->min 3->snd biggest 4->snd smallest 5->simul.Line
   
   
                       
    putStr (unlines (optimLine))
    let soraus = (optimLine)
    let addings = let auswahlIn2 t w =  (drop (t-1) (take t (lines w)))

                  in let ridString2 t w= let ins1 = map ord (concat(auswahlIn2 t w))
                                         in let ins2 = filter (/=34) ins1 -- filter alle ' [ ' und '] '
                   -- in let ins3 = filter (>=32) ins2
                                         in (map chr ins2)
                  in let filtKlam2 w t = let fofilt t =  map chr(cvsZeilenBreak ((ridString2 t w)))
                                           in (map fofilt t)
                  in let neLengte = length (lines theRealS)
                  in let tustaP1 w = (filtKlam2 w  [1..neLengte]) -- for write new WX
                  in let foGraph w = outPutMaximafunct (tustaP1 w) [1..neLengte] 
--------------------------------------------------------------------------------
                  in let foMAx2 = (foGraph the10SimuS)
                  in let foReal =  (foGraph theRealS)
                --  in let optimiZed = foGraph (unlines soraus)
                  in let writeWXFile2 = (aCompleteWX ( [(foMAx2),(foReal)]) [[1,2,3],[4,5,6]] "20" "0.49" "0.60")
                  in let justwriteOnes = zeitraum 
                  in if (writeE==1)&&(neLengte==( zeiLLe))
                  then do 
                        writeFile (root++"theNewSimu.wxm") writeWXFile2
                        (print "Write_Mode_WX: wrote \"theNewSimu.wxm\"") 
                          
                  else if (writeE==2)&&(neLengte==( zeiLLe))
                  then do
                           --  writing the Series for Iteration -> loopSimus
                        writeFile (root++"aaKA/aakaLRun"++( simuWrite)++".txt") ("")
                        avanti ["yo"]
                               -- (print ("Write_Mode_: building \"aaKA/aakaLRun"++(simuWrite)++".txt\n needs furtehr adding enter 'writeE' == 3"))  
                  else if (writeE==3) 
                  then do
                        --  when adding then the data should be put into into the most actual
                        --  "aaKA/aakaLRun ++ token .. file
                        let foAdds = let stes1 = ( simuWrite)
                                      in  (root++"aaKA/"++writeToKA++( stes1)++".txt")
                        let foAdds2 = let stes1 = (concatM(unwords soraus))
                                      in stes1
                         --  machinsert foAdds e z 
                        putStrLn "add SUSTITUTE" -- add [foAdds,foAdds2]
                        avanti ["yo"]
                           
                       --    (print ("Adding-Mode: building \""++root++"aaKA/aakaLRun.txt\""))                        
                  else do
                        avanti ["yo"] --(print ("Just showing Mode"))
    addings
   -- let neueSieb = ( furtherSor)
  --------------------------------------------------------------------------
-- Gaussian influenced - Aehnlichkeitswarscheinlichkeit
--- this function rates the similarity of two
-- Lists vergleicht Aehnlichkeit und gibt 
-- die Prozentzahl der einer stelle 
-- der beiden listen aus
--
foaehnli1 a b = (a-b)
foaehnli2 a b = (b-a)
--umrechnen in % mit a < b und b > a  
aehnlichF a b = let a1 = if a > b then ((foaehnli1 a b)/ (a/100) )
                         else if a<b then  ((foaehnli2 a b)/ (b/100) )
                         else 0
                in let b1 g h = ((g) / ((h)/100)) 
                in a1 

 
similaritYvalue li la = let a = la -- z.b. [11,12,34,44]
                        in let b = li 
                        in let c1 w = sum w
                        in let c2 = c1 a--length
                        in let c3 = c1 b--length
                        in  aehnlichF c2 c3 -- in let d = 


 











