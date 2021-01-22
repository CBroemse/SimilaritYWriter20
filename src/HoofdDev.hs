{- this module is a layer to                         color-sceme: morning
 turn all data of this programm into workable
 functions that will be exported into main -}
--
-- with mayor modules:
-- ------------------
-- Colored_2_3_5_counter.hs:  plot 2d ,3d, 2d/3d,sort network,  
--                            mq - ptc - fourierFS functions,
--                            busschop-bradley, display trivial machine
--
-- Experiment3:                randomize input, guesses , find solutions,
--                            2 work flows: random number=> baysian type 
--                                 from li and addGH    => baysian type
--                            4 streams in and out of cells:
--                               based on the 2 work fows
--                             => cell I,cell II cell III, cell IV
--                            other destinct work flow based on ptc9
--                            => cell1 , cell2, cell3, cell4
--                            1 formation role-model : runEXP3 && runEXP3Char
--
-- TheAtoBplotter   library to plot wxmaxima graphs . functions to compare
--
-- FourierFieldCLASSIFIER    provides basic magic quadrants or mq-functions
--                            to be used in file "src/Col..20.hs" 

-- UsefulFunctions19 library of useful functions used by all mayor modules
-- Path2Val2        introduce path svg data type not used in this module yet
--
module HoofdDev() where
--prelude
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
-- own modules 
import qualified Colored_2_3_5_Counter20 as C
import qualified Experiment3 as E
import qualified DataTypeBayes as BT
import UsefulFunctions19
import DataTypePunkt
   --, Experiment3
   --, TheA
   --
--some functions
ausw = C.ausw
checkflow = E.checkflow
triggerWord = E.triggerWord
inActie = E.inActie
baysianTypeInt=E.baysianTypeInt
baysianTypeString=E.baysianTypeString
--
--
--           ############### whole function is ################################## SAME AS   cellStream3EXT- 13-1-21        
                                                         --  ########################## BOTH are an INDEX matrices (or simply values)
       -- domain2 syntax                                                                                   ##############   I: EXPORT TO MAIN
       --                                                                                                                  II:  PUNKT JUNCTION POINT 
       --                                                   III: JOIN with a. 2dplot, 3dplot, 2/3dplot, ptcChart.html
       --                                                                                                    in Main
       --                                                                                                                  IV: EXPORT BAYSIAN type to MAIN 
       --                                                                                                                      make nice exlpainatory interactive 
       --                                                                                                                   V: 1-1-21 issue 'story line' join the lose ends
       --                                                                                                                      with 'Colored_2_3_5_Counter20.hs'=C20 see overview
       --                                                                                                                         found most versataile ptc6 ptc7? , ptc9
       --                                                                                                                         see solutions of 'Quirky-Example' this lP function
       --                                                                                                                         in C20 ; mix wit first 'C.vb' example
       --                                                                                                                         => writes 'src/2dpgfunctions.wxm" 
       --                                                                                                                            watch an ordered state and the ptc data 
       --                                                                                                                            and the purple solution function which main aim
       --                                                                                                                            is to turn a very ordered state into -> 
       --                                                                                                                            -> [[Char]] -> [[Int]] -> ruEXP3 =
       --                                                                                                                            => relation -> fourierFS to mq-functions
       --                                                                                                                  VI: THE NEW CELL TYPE as basis ptc9 is involved 
       --                                                                                                                      with a rating of guesses AFTER PUNKT JUNCTION POINT
       --                                                                                                                      in Main.
------------------------------------------------------
-- SIMIVAL RUNS for  'Quirky example'
-- manifest the given li function with and without 'appropiate mathematical syntax'
-- give a simiVal list to compare both states
-- liSolution: a real Prior/Bias?!? 
-- e.g -
li1 = ["0*x + y + 0*z = 3","0*x + y + 0*z = 31*x + 0 + 0*z = 6","1*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
li2 = ["0*x + y + 0*z = 3","1*x + 0 + 0*z = 6","0*x + 0*y + z = 2","x + y + z = 11"]
simiVals bonelist pick1 pick2 punktList  = 
     let foChain = length bonelist
     in let makePalette pick1 pick2 punktList togoToList togtoList2  = noSense togoToList togtoList2
                       where
                          dada fopick fodeze = head (ausw (read fopick) fodeze);
                          noSense deze deze2 = Punkt (dada pick1 deze) (Just (maybePu (dada pick2 deze2))) Nothing Nothing Nothing Nothing;
     in let gaussRate eins zuI = C.similaritYvalue eins zuI
     -- ---------------------------------------------------------------------------------
     in let beRepKEYRAW pick1 pick2 onelist twoList punktList = (crunchtoSimi pick2) 
                      where
                  --      commands =   [bone1,bone2,bone3,bone4,bone5]; 
                        compar = head (ausw (read pick2) twoList);
                        paletteKEY fpic2 twoList astrList = makePalette pick1 fpic2 astrList onelist twoList;
                        cleanPaletteRaw fpic2 twoList astrList = checkflow[] [(paletteKEY fpic2 twoList astrList )];  -- go to pick2 
                        crunchtoSimi fpic2  = let sta1 d = map ord d  
                                              in let sta2New = let ste1 = (map realToFrac (sta1 (unwords(cleanPaletteRaw fpic2 twoList punktList))))
                                                                in drop 1 (take (length ste1) ste1)
                                              in let sta3New = map realToFrac (sta1 ((compar))) -- *****************************SELECT EACH LINE OF GUESS
                                              in gaussRate sta3New sta2New

     in let beRepKEY pick1 pick2 punktList = (beRepKEYRAW pick1 pick2 commands commands punktList  )-- makePalette pick1 pick2 punktList commands
                      where
                        commands = bonelist; 
     in beRepKEY pick1 pick2 punktList

-- so far liSolution is merely a symbolic type it has no use other to be simivalued to other liSolutions 
runEXP3 li liSolution pi ghAdd = do                                                         
       let iDF = li --["AaEe0","AaEeI","i0000","OoUu0","OoU0Y","y0000"]                                               
       let lP = liSolution --["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
       let choosDomain ff =  if ff == 1 then iDF 
                             else lP 
       let chD ff  = choosDomain ff
       allforIV <- forM [1] (\four4 -> do
            -- first loop is 'IDF' second one is 'LP'
            let de e r = head $ ausw e r                        --------- get every list of li or liSolution
            let foaLi e =  de e (chD four4)
            let aLi = [(foaLi 1),(foaLi 3),(foaLi 4),(foaLi 6)] --------- set to work with li lists of length 6
            let fg = let allFor = (E.kArmWORK 1 ["no onfluence"] aLi 1 pi 1 1 [] ghAdd) --(runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 3 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)))
                     in let toHaal =  charFilterInt ghAdd;
                     in  if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "intern" ) == "1" then (fst allFor)
                       --  else if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "unsort" )  == "1" then nub [unsortedStream] -- (map read [(triggerWord "cell" "cell3")]) -- snd (ghAdd,(snd(charFilterInt ghAdd)))
                         else (head (snd allFor))
            fg     
            return (fg))
       -- apply an algorythm
       let beforeAlgo  = allforIV -- if (triggerWord (unlines(checkflow [] [pi]))) == 1 then (map fst allforIV)
                       --  else ((map snd allforIV))
       --let beforewithGH = head $last $ head $ allforIV
       (return (beforeAlgo))
-----------------------------------------------------------------------------------------------------------------------
-- *HoofdDev> let pi0 = Punkt "unsort" Nothing Nothing Nothing Nothing Nothing
-- e.g let liM2 = ["00000","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","01111","xyz=11"]
--     let pi = Punkt "intern" Nothing Nothing Nothing Nothing Nothing
--  fillBayesian:Int , select export function with 'choosF' set to 2 
-- *HoofdDev> (formHoofdEX1 liM2 ["CEL"] pi0 "P" fillBayesian)  
formHoofdEX1WORK foli4 stringBT pi ghADD fillBayesian =  do
                                        let foliOrder = [1,2,536,537,538,1073,1074,1075,1076,1610,1611,1613]
                                        let runRandomLin foli4 = nub(map (E.cellStream3 foli4) foliOrder)
                                        let rndLin = runRandomLin foli4
                                        let usage h = (head$ausw h rndLin)
                                        let tHisOrder = if (even(length foliOrder))==False then map usage [1] --map mapKick [1,2,3,4,5,6]
                                                        else map usage [2,4,6,8,10,12]
                                        let takeRndChars n = zufallsBasic1 1 (length(usage 1)) n -- finally select an rand Char with n + 1 
                                        let rndInt n =  last (zufallsBasic1 (n+1) 9 n)
                                        let aTree n = if (length$rulerBT (head foli4) ghADD) == (length ghADD) then do takeRndChars n --map takeRndChars [1..50] --runRandomLin foli4
                                                      else takeRndChars n  --stream2 > streamI
-- I. MAIN DATA STREAM: 'pingWORK'   -- just depends on foli or on guesses (intern/extern)  pvs:[prog variables] e.g see above  ******************************************************************************* CHANGE TYPE ORDER
                                        let pingWORK io fofoli guesses importFunc = checkflow io (E.inActieRAW ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"] fofoli guesses importFunc) -- pvs can be mixed up to change order
                                        let newLayerRAW r = group$ unwords $ pingWORK [father] E.li8 ["guesses"] r
                                      ----------------------------------------------------------------------------------------------
-- II. MOUNT CELL I..IV : 'workSlotRAW'
                                        let workSlotRAW streamI streamII  io1 = BT.checkCELLs io1 streamI streamII   -- ############################################ mounted  bayesCELLI..IV with BT.checkCELLs
                                        let workSlot io1 =  workSlotRAW "cell" "0" io1
                                        let allSlot = map workSlot (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
                                        let allSlotWORK streamI streamII = map (workSlotRAW streamI streamII) (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
{- III. RATE LI AND GUESS:     -}       let plugExpWORK fofoli4 k guess a b = runEXP3 fofoli4 (ausw k (concat (allSlotWORK a b))) pi guess  -- ######################## COMBINES change li and BAYES Type 'new cell' 
                                        let unsortedStream  aLi left right = (simiVals aLi left right pi) -- simi rate everthing UNSORTED
                                        let abyssDevide = let step1 = map chr $ filter (>=65) (usage 2)   
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (ausw a) step1 
                                                          in map switch [1..(toRead)] 
                                        let abyssDevide2RAW = let step1 = map (map chr) (sort$ group $ filter (>=65) (usage 2))   
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]
                                        let abyssDevide2 = head abyssDevide2RAW
                                        let rndLi = [abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2]
                                        let type1 =  (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "xyz=11" "dddd" "CELLONE" )  -- type 1 intern change li
                                        let type2 =  (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "dddd" "CELLONE" "CELLONE" ) --type 2 change guess
           ----------------------------------------------------------------------------------------------------------------------------------------------
                             --  similar to checkCELLs
                                        let breedCELLs io a b sixBreadCells = checkflow io $ BT.joinBayes a b (["cell"] ++ sixBreadCells ++ ["CELLONE","CELLTWO","CELLTHREE","CELLFOUR"])
                            -- same as above but can match "cell" from [] name 
                                        let breedCELLPunkt io a b sixBreadCells = checkflow io $ BT.nameNewCell "cell" a b ( sixBreadCells)
                           -- insert data into new Punkt  retrieve from     a ->
                           --                                      or a and b ->
                                        let bayesianastigmatic a b = (BT.bayesAstigmatic a b) \\ "cell"
                                        let doer fu n = if (fu ) == " cell" then let step1 = "cell" ++ (show$rndInt n)
                                                                                    in step1
                                                         else fu 
                                        let workPunkt n = doer (concat(breedCELLPunkt [] "B" "B" (abyssDevide2))) n
                                        let workPunktRAW io a b forBreadCells n = doer(concat(breedCELLPunkt io a b forBreadCells)) n
                                        let bootPunkt p = (head$ausw p abyssDevide2RAW)
                                        let type1or2 guess lineN atom = (plugExpWORK  (map concat(map bootPunkt [1..6])) 1 guess (head$ausw atom (bootPunkt lineN)) (head$ausw atom (bootPunkt lineN))) --type 2 change guess
                                        let mapWithAbyss2 io a forBreadCells b = map (b++) (map (workPunktRAW io a b forBreadCells) [1..6])
                                        let littleWriter io a b forBreadCells = if  (workSlotRAW a b [mother]) == [" 1"] then mapWithAbyss2 io a forBreadCells b --"write data to CELL" ---------------MATCH THEN WRITE TO CELL
                                                               else ["not"] 
                                        let ratE io line2 randOm simple lineN atom = nub(type1or2 (workPunktRAW io  (concat (bootPunkt line2 )) simple (head(ausw atom (ausw lineN abyssDevide2RAW))) randOm ) lineN atom) -- simi rate
                                        let ratEChars io line2 randOm simple lineN atom = nub(type1or2 (workPunktRAW io  (ausw atom (concat (bootPunkt line2 ))) simple (head(ausw atom (ausw lineN abyssDevide2RAW))) randOm ) lineN atom) -- simi rate
                                        let guessWithBias n =   E.poolBandNotB E.li4 n -- as four bread cells plug into breedCells
                                        putStrLn "Enter: 0 to close"
                                        let loop = do
                                           --  miIO <- getLine
                                          --   foB <- getLine
                                          --   kk <- getLine
                                             out <- getLine
                                            -- let gonz some = read some 
                                             let guesseS n = E.poolBandNotB E.li4 n -- as four bread cells plug into breedCells
                                            -- let lab io a b guesseS = breedCELLs io a b guesseS  -- write guesses into a cell system
                                          --   bios <- forM [1..9] (\bio -> do
                                            -- lineN:Int, line of [abyssDevide2] , atom:Int, of lineN, atomGuess:Int,  
                                                  
                                                --  inner <- return (ratE 1 1 1)
                                            --      teest <- getLine 
                                               --   let ratE guess lineN atom = show (plugExpWORK  (map concat (map bootPunkt [1..6])) lineN guess (head$ausw atom (bootPunkt lineN))) 
                                              --    return (ratE bio teest 1 1))
                                            -- putStrLn $show bios  
                                           --  if show ion > ("50") then loop                      -- write Bayes
                                             if (read out)> 0 then do 
                                                            er <- getLine
                                                            at <- getLine
                                                           -- io <- getLine
                                                          --  let checkCell atomGuess randOm = ratE (read out) 1 1 1 -- (workPunktRAW [mother] (unline(bootPunkt 1)) "B" ["B"]  randOm )
                                                          --   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                                          --                        n   atom2    line2      rnd?     aGuess 
                                                            putStrLn $show (ratE [] (read out) (1) "B" (read out) 1) -- (read at))
                                                            putStrLn $show (ratE [] (2) (2) "BBBB" (read out) 2) -- (read at))
                                                            putStrLn $show (ratE [mother] (1) (3) "cell" (read out) 1) -- (read at))
                                                            putStrLn $show (ratEChars [] (1) (7) "cell" (read out) 3) -- (read at))
                                                            putStrLn $show (ratEChars [] (1) (7) "cell" (1) 3) -- (read at))

                                                            hu <- getLine 
                                                            putStrLn $show (ratE [mother] 1 1 hu 1 1) -- (read at))
                                                            putStrLn $show (ratE [mother] 2 3 ("cell"++hu) 1 1) -- (read at))
                                                            putStrLn $show (ratE [father] 2 2 hu 2 1) -- (read at))
                                                            putStrLn $show (ratE [mother2] 3 2 hu 3 1) -- (read at))
                                                            putStrLn $show (ratE [loopNumber] 4 2 hu 4 1) -- (read at))
                                                            putStrLn $show (ratE [minMaxTrueOrFalse] 5 2 hu 5 1) -- (read at))


                                                            loop
                                             else do putStrLn $"done"
                                        loop 
                                      -- line:Int ; select a String from abyssDevide2RAW    #############################  COULD SPLIT foli4 instead of                           
    {- MAIN BRANCH SET TO:       -}     let tailRAW line = (concat(ausw line abyssDevide2RAW))    -- #######   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                        let oneCellAbyss2 io a sixBreadCells n = map (mapWithAbyss2 io a sixBreadCells) (tailRAW n) 
                                        let abyssDevide2TEST singularitY = let step1 =  concat $map guessWithBias $ map read (tails singularitY \\ [""])-- (usage 2)  
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]
                                        let iterAby =  let step1 =    map (map chr)  (abyssDevide2TEST "12341234") -- (usage 2)  
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]
                                        let cellLogic  line stream  = map (take 3 )(concat  (ausw line( (stream)))) 
                                        let littleWriter2 io a b forBreadCells n atom = let choose pick2 pick1 = (ausw pick1 (concat (ausw pick2 iterAby))) 
                                                                                        in let bootcells forBreadCells k = (mapWithAbyss2 io a forBreadCells k) 
                                                                                        in let celLogic io atomN line = (oneCellAbyss2 io "Z" (concat (ausw atomN abyssDevide2RAW)) line) --cellLogic  1 3 (oneCellAbyss2 io "JLLNP" (bootPunkt (read n)) line)
                                                                                        in let steps foatom = (cellLogic foatom (celLogic io atom (read n))) -- ######################### EXPORT 
                                                                                        in  map concat$nub$(transpose) [(concat(map steps [1..6])),  (concat( (ausw (read n )iterAby)))]  --writeRNDcellInput (6) (read n) -- zipWith (++) (steps (read n)) (concat(ausw atom (ausw (read n )iterAby)))
                                        let compareCells io a b forBreadCells n atom = let choose pick2 pick1 = (ausw pick1 (concat (ausw pick2 iterAby))) 
                                                                                        in let bootcells forBreadCells k = (mapWithAbyss2 io a forBreadCells k) 
                                                                                        in let celLogic io atomN line = (oneCellAbyss2 io "Z" (concat (ausw atomN abyssDevide2RAW)) line) --cellLogic  1 3 (oneCellAbyss2 io "JLLNP" (bootPunkt (read n)) line)
                                                                                        in let steps foatom = (cellLogic foatom (celLogic io atom (read n))) -- ######################### EXPORT 
                                                                                        in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",concat(steps line )] 1 "dddd" "CELLONE" (head (bootPunkt line)) )
                                                                                        in let type1or2 guess lineN atom = (plugExpWORK  (map concat(map bootPunkt [1..6])) 1 guess (head$ausw atom (bootPunkt lineN)) (concat(steps lineN))) 
                                                                                        in  (type1or2 (concat(ausw (read n) foli4)) (read n) atom)
                                        
            -- compare booted cells == bootPunkt with guesses, random Numbers in cells and foli4
                                        let compareCells io a b forBreadCells n atom = let choose pick2 pick1 = (ausw pick1 (concat (ausw pick2 iterAby))) 
                                                                                        in let bootcells forBreadCells k = (mapWithAbyss2 io a forBreadCells k) 
                                                                                        in let celLogic io atomN line = (oneCellAbyss2 io "Z" (concat (ausw atomN abyssDevide2RAW)) line) --cellLogic  1 3 (oneCellAbyss2 io "JLLNP" (bootPunkt (read n)) line)
                                                                                        in let steps foatom = (cellLogic foatom (celLogic io atom (read n))) -- ######################### EXPORT 
                                                                 --     in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat (usage line))] 1 "dddd" "CELLONE" ) ) 
                                                                                        in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",concat(steps line )] 1 "dddd" "CELLONE" (head (bootPunkt line)) )
                                                                                        in let type1or2 guess lineN atom = (plugExpWORK  (map concat(map bootPunkt [1..6])) 1 guess (head$ausw atom (bootPunkt lineN)) (concat(steps lineN))) 
                                                                                        in  (type1or2 (concat(ausw (read n) foli4)) (read n) atom)
                                        putStrLn$ show$ map (map chr) (abyssDevide2TEST "12341234")
                                        putStrLn$show $(iterAby)
                                     --   let boa = do
                                       --    io <- getLine                   --   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                        putStrLn$show$(littleWriter2 [] "aaa" "1" (concat (ausw 4 abyssDevide2RAW)) "1" 1) 
                                        putStrLn$show$(littleWriter2 [] "aaa" "1" ((ausw 1(concat (ausw 1 abyssDevide2RAW)))) "2" 1)
                                        putStrLn$show$(littleWriter2 [mother] "aaa" " B" ((ausw 1(concat (ausw 1 abyssDevide2RAW)))) "1" 1) 
                                        putStrLn$show$(littleWriter2 [loopNumber] "aaa" "B" ((ausw 4(concat (ausw 4 abyssDevide2RAW)))) "1" 2) 
                                        putStrLn$show$(littleWriter2 [mother2] "nothing" " F" ((ausw 3(concat (ausw 3 abyssDevide2RAW)))) "3" 3)
                                        putStrLn$show$(littleWriter2 [mother] "nothing" " J" ((ausw 1(concat (ausw 3 abyssDevide2RAW)))) "3" 1)
                                        putStrLn$show$(littleWriter2 [loopNumber] "nothing" "P" ((ausw 1(concat (ausw 4 abyssDevide2RAW)))) "4" 1)
                                        putStrLn$show$(littleWriter2 [father] "nothing" " J" (((concat (ausw 3 abyssDevide2RAW)))) "3" 2)

                                        let choosF n = head$ ausw n [ show (oneCellAbyss2 [mother2] "Z" (concat (ausw 3 abyssDevide2RAW)) 3),show(littleWriter2 [mother] "nothing" "J" (((concat (ausw 3 abyssDevide2RAW)))) "3" 2),show(compareCells [mother] "nothing" "J" (((concat (ausw 3 abyssDevide2RAW)))) "3" 2),show(type1or2 (unlines ["ddd"]) 1 1),show(workPunktRAW [mother] (head(bootPunkt 2 )) "B" (head(ausw 1(ausw 1 abyssDevide2RAW))) 1 ){-get a cell (char) of one selected adyssDevide-}] 
                                        return (BT.formationB [((choosF 3))]) --fillBayesian


   where
      palette = runEXP3 foli4 stringBT pi ghADD; 
      choosDomain ff =  if ff == 1 then foli4 
                                   else stringBT; 
      chD ff  = choosDomain ff;
-- slotDomain:String, map a li; testMap:String, map various ghADD -- write to stringBT ? 
      rulerBT slotDomain testMap = baysianTypeString [""] slotDomain testMap;  -- test with right length the order of ghADD 
      protractorBT slotDomain strIntBT testMap = baysianTypeInt ["48","","","","","57"] slotDomain strIntBT testMap;
 -- map with filterB 
      actie pv1 pv3 pv4 pv6 foli ghAdd = inActie (pv1 pv3 pv4 pv6 foli [ghAdd]);
      filterB = map (filter (/='A')) E.li4;
      solong b = [1..(length b)];
 --which:Int length output; m: source list; fromA: Int choose from m
      soSame  which m fromA = let stap1 = head(ausw fromA m)
                 in take which [stap1,stap1..];
      bayesMonad b thisAction = do 
          let solang = solong b
          bayWorld0 <- forM solang (\bay -> do
              let outBayesMonad = thisAction
              return (outBayesMonad))
          bayWorld0; 
      typeCheckTriggerword = if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "intern" ) == "1" then palette 
                              else if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "cell" )  == "1" then (map read [(triggerWord "cell" "cell3")]) -- snd (ghAdd,(snd(charFilterInt ghAdd)))

                         else palette --(head (snd allFor))


                   



-----------------------------------------------------------------------------------------------------------------------
-- THERE ARE FILLED IN MATICES AND UNFILLED ONES = > 'cellLogic'
-- stringBT:[String]  string bayesian type carries name
-- TWO streams 1. depend on LAST ATOM of list
--             2. depend on ghADD             => both get simivalued
--
 --                     allSlotRAW -> simi rate   intern  -> connected foli4 with simi rate  -> write with plugExpWORK   -> tweak 1 new layer likethis    
                        --                                   via runEXP3                                             
                        --                  map workSlot    
                        --                                               extern  -> connected foli4 with bayes types
                        --                                                          AND connected to ghADD   joinBayes
--------------------------------------------------------------------------------------------------------------------
--                         make CELLs : 'allSlot'       |       make new cells: 'plugExpWORK'
--  search cells                      (allSlotWORK "CELLONE" "CELLONE" )
--  find cell                         (workSlotRAW "cell" "cell1" [mother]) 
                       
--  fill data into cell               type1or2  
-- *HoofdDev> let pi0 = Punkt "unsort" Nothing Nothing Nothing Nothing Nothing
-- e.g let liM2 = ["00000","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","01111","xyz=11"]
--     let pi = Punkt "intern" Nothing Nothing Nothing Nothing Nothing  
-- *HoofdDev> (formHoofdEX1 liM2 ["CEL"] pi0 "P")                                             
formHoofdEX1 foli4 stringBT pi ghADD =  do
                                        let foliOrder = [1,2,536,537,538,1073,1074,1075,1076,1610,1611,1613]
                                        let runRandomLin foli4 = nub(map (E.cellStream3 foli4) foliOrder)
                                        let rndLin = runRandomLin foli4
                                        let usage h = (head$ausw h rndLin)
                                        let kickThisOF kick this = (map ord kick) \\ (map ord this) -- this list without that  
                                -- take n many solutions in this order
                                        let tHisOrder = if (even(length foliOrder))==False then map usage [1] --map mapKick [1,2,3,4,5,6]
                                                        else map usage [2,4,6,8,10,12]
                                        let takeRndChars n = zufallsBasic1 1 (length(usage 1)) n -- finally select an rand Char with n + 1 
                                        let rndInt n =  last (zufallsBasic1 (n+1) 9 n)
                                        let aTree n = if (length$rulerBT (head foli4) ghADD) == (length ghADD) then do takeRndChars n --map takeRndChars [1..50] --runRandomLin foli4
                                                      else takeRndChars n  --stream2 > streamI
-- I. MAIN DATA STREAM: 'pingWORK'   -- just depends on foli or on guesses (intern/extern)  pvs:[prog variables] e.g see above  ******************************************************************************* CHANGE TYPE ORDER
                                        let pingWORK io fofoli guesses importFunc = checkflow io (E.inActieRAW ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"] fofoli guesses importFunc) -- pvs can be mixed up to change order
                                        let newLayerRAW r = group$ unwords $ pingWORK [father] E.li8 ["guesses"] r
                                      ----------------------------------------------------------------------------------------------
-- II. MOUNT CELL I..IV : 'workSlotRAW'
                                        let workSlotRAW streamI streamII  io1 = BT.checkCELLs io1 streamI streamII   -- ############################################ mounted  bayesCELLI..IV with BT.checkCELLs
                                        let workSlot io1 =  workSlotRAW "cell" "0" io1
                                        let allSlot = map workSlot (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
                                        let allSlotWORK streamI streamII = map (workSlotRAW streamI streamII) (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
{- III. RATE LI AND GUESS:     -}       let plugExpWORK fofoli4 k guess a b = runEXP3 fofoli4 (ausw k (concat (allSlotWORK a b))) pi guess  -- ######################## COMBINES change li and BAYES Type 'new cell' 
                                        let unsortedStream  aLi left right = (simiVals aLi left right pi) -- simi rate everthing UNSORTED
                                        putStrLn "use 'workSlot' distribute CELL I..IV to simi vals"
 --    RATE any A and B                      k:Int; select from type list        |   pi Punkt -> String     |
                                        --let distCELL k a b = (unsortedStream  (ausw k (concat (allSlotWORK a b))) (concat(checkflow [] [pi])) b)  -- select from list -> make cells -> rate 
                                      ----------------------------------------------------------------------------------------------                                          
                                        putStrLn (show(workSlotRAW "CCCC" "dddd" [mother]))
                                        putStrLn (show(workSlotRAW "CCCC" "A1" [mother]))
                                        putStrLn (show(workSlotRAW "cell" "cell1" [mother]))
                                        putStrLn (show(workSlotRAW "CELLONE1" "CELLONE" [father]))
                                        putStrLn (show(workSlotRAW "CELLONE1" "CELLONE1" [father]))
                                        putStrLn (show(workSlotRAW "CELLONE" "cCELLONE11" [father]))
                                        putStrLn (show(allSlotWORK "CELLONE" "CELLONE" ))
                                                                               
                                        putStrLn (show (plugExpWORK foli4 1 "xyz=11" "CELLONE" "CELLONE" ))  ----simi rate this foli4"
                                        
                                  --      putStrLn (show(breedCELLs [father] "CELLONE" "CELLONE"))
                                   --     putStrLn (show(breedCELLs [mother] "CELLONE" "CELLONE"))
                                     --   putStrLn (show(breedCELLs [mother2] "CELLTWO" "CELLTWO"))
                                      -- => do one pingWORK => then change li 
                                        putStrLn "newLayerRAW"
                                        putStrLn$ show$newLayerRAW 1
                                        putStrLn "try this order tHisOrder"
                                        putStrLn$ show $ map chr (usage 2)    
                                        putStrLn$ show $  map chr $ filter (>=65) (usage 2)   
                                        let abyssDevide = let step1 = map chr $ filter (>=65) (usage 2)   
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (ausw a) step1 
                                                          in map switch [1..(toRead)] 
                                        putStrLn$concat abyssDevide
                                        putStrLn "HERE"   
                                        putStrLn$show$group$sort$usage 1   
                                        let abyssDevide2RAW = let step1 = map (map chr) (sort$ group $ filter (>=65) (usage 2))   
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]
                                        putStrLn $ show abyssDevide2RAW 
                                        let abyssDevide2 = head abyssDevide2RAW
                                        putStrLn$show$abyssDevide2 
                                        putStrLn$show$concat$abyssDevide2                   
                                        putStrLn$show$group$abyssDevide2
                                        putStrLn$show$sort$abyssDevide2  
                                        let rndLi = [abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2,abyssDevide2]
                                        putStrLn (show (plugExpWORK (abyssDevide2) 1 (concat abyssDevide2) "CELLONE" (concat abyssDevide2 )))  ----simi rate this foli4"
                                        putStrLn (show (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "xyz=11" "CELLONE" "dddd" ))  ----simi rate this foli4"
                                        putStrLn (show (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "xyz=11" "dddd" "CELLONE" ))  ----simi rate this foli4"
                                        putStrLn (show (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "dddd" "CELLONE" "CNE" ))  ----simi rate this foli4"
                                        let type1 =  (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "xyz=11" "dddd" "CELLONE" )  -- type 1 intern change li
                                        let type2 =  (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "dddd" "CELLONE" "CELLONE" ) --type 2 change guess
           ----------------------------------------------------------------------------------------------------------------------------------------------
     {- RATE CELLS UNSORTED -}          putStrLn (show(simiVals ["cell","CELLONE2","CELLTWO","CELLTHREE","CELLFOUR"] "1" "2" pi)) -- "cell" "cell1" ))
                                        putStrLn (show(workSlotRAW "cell" "cell26.45" [mother])) 
                                       
                                        -- if lenght sixBreadCells > 4 ,output not availabe via mother etc.
                             --  similar to checkCELLs
                                        let breedCELLs io a b sixBreadCells = checkflow io $ BT.joinBayes a b (["cell"] ++ sixBreadCells ++ ["CELLONE","CELLTWO","CELLTHREE","CELLFOUR"])
                            -- same as above but can match "cell" from [] name 
                                        let breedCELLPunkt io a b sixBreadCells = checkflow io $ BT.nameNewCell "cell" a b ( sixBreadCells)
                           -- insert data into new Punkt  retrieve from     a ->
                           --                                      or a and b ->
                                        let bayesianastigmatic a b = (BT.bayesAstigmatic a b) \\ "cell"
                                      
                                    -- STAGE 2 
                                        putStrLn "abbyssDevide2"
                                        putStrLn $show abyssDevide2
                                        putStrLn$show$breedCELLs [father] "B12" "B" (ausw 1 abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "B" "B" (abyssDevide2)
                                        putStrLn$show$breedCELLPunkt [] "B" "B" (abyssDevide2)

                                        putStrLn$show$breedCELLs [father] "B1" "B1" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "B" "B12" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "E" "E" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "" "E" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "" "D" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "D" "D" (abyssDevide2)
                                        putStrLn$show$breedCELLs [minMaxTrueOrFalse] "D" "D" (abyssDevide2)
                                        
                                        putStrLn$show$breedCELLs [father] "BBBDD" "BBBDD" (abyssDevide2)
                                        putStrLn$show$breedCELLs [father] "B" "B" (ausw 1 abyssDevide2)
                                        putStrLn$show$breedCELLs [loopNumber] "D" "B" ( abyssDevide2)
                                        putStrLn$show$breedCELLs [loopNumber] "F" "D" ( abyssDevide2)

                                        --    make breedCELLs (big Chars)   ->  insert by abyssDevide2
                                        --
                                        --      check  simiVal 
                                        --        in type1 or type2 : 
                                        --        change guess ____\    takeRndChars  _____\   match via
                                        --        or change li     /    rndInt n           /   abyssDevide2 ->
                                        --
                                        --         wrote cell e.g  [["B","B","B"],["D","D"],["F"]]  <=> abyssDivide2 => ["B","B","B","D","D","F"]
                                        --
                                        --         add rndInt n  insert e.g workPunktRAW 1  => "cell6" 
                                        --
                                        --                       insert as  "Bcell6" ,..
                                        let doer fu n = if (fu ) == " cell" then let step1 = "cell" ++ (show$rndInt n)
                                                                                    in step1
                                                         else fu 
                                        let workPunkt n = doer (concat(breedCELLPunkt [] "B" "B" (abyssDevide2))) n
                                        let workPunktRAW io a b forBreadCells n = doer(concat(breedCELLPunkt io a b forBreadCells)) n
                           -- match from every abyssDevide if B is part of guess
                                        let bootPunkt p = (head$ausw p abyssDevide2RAW)
                  -- let type2 =  (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat abyssDevide2)] 1 "dddd" "CELLONE" "CELLONE" ) --type 2 change guess
                  --             with map (concat)    ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                  --              COMPARE NEW cellnames, see above to guess 
                                        let type1or2 guess lineN atom = (plugExpWORK  (map concat(map bootPunkt [1..6])) 1 guess (head$ausw atom (bootPunkt lineN)) (head$ausw atom (bootPunkt lineN))) --type 2 change guess
                                        
                                      --  let workPunktRAW  k1 k2 n  = doer (concat(breedCELLPunkt [] (head(ausw k1 abyssDevide2)) (head(ausw k2 2abyssDevide2)) (abyssDevide2))) n
                                        putStrLn$ show$ map concat ((map bootPunkt [1..6]))  --(type1or2 "ddd" 1 1)
                                        putStrLn$show (workPunktRAW [] "B" "B" abyssDevide2 1)
                                        putStrLn$show$ (type1or2 (unlines ["ddd"]) 1 1)
                                     
                                        let mapWithAbyss2 io a forBreadCells b = map (b++) (map (workPunktRAW io a b forBreadCells) [1..6])
                                        let littleWriter io a b forBreadCells = if  (workSlotRAW a b [mother]) == [" 1"] then mapWithAbyss2 io a forBreadCells b --"write data to CELL" ---------------------------------MATCH THEN WRITE TO CELL
                                                               else ["not"] 

       -- atomGuess: Int, of a selected abyssDevide2 -> [String]  -------------------------------########################################################################## MAIN DATA --->  RATE  
       -- simple: String, guess compare to atomGuess
       -- -----------------------------------------------------------------------------------------
       --     new cell: abyssDevide2                                                       STAGE II
       --    -----------------------   
       --       EXP3   ->      workPunktRAW  <=>  add values to cell via 'doer'        plugExpWORK
       --                        ||                                                                    evaluate 
       --                      
       --                        \/                                                     
       --  -----------------RATE devideAbyss2 -> rate String------------------------------------------
       --    CELL I..IV                                                                    STAGE I
       --   -------------
       --       EXP3  ->      allSlot
       --
                                        let ratE io line2 randOm simple lineN atom = nub(type1or2 (workPunktRAW io  (concat (bootPunkt line2 )) simple (head(ausw atom (ausw lineN abyssDevide2RAW))) randOm ) lineN atom) -- simi rate
                                        let ratEChars io line2 randOm simple lineN atom = nub(type1or2 (workPunktRAW io  (ausw atom (concat (bootPunkt line2 ))) simple (head(ausw atom (ausw lineN abyssDevide2RAW))) randOm ) lineN atom) -- simi rate

                                        let guessWithBias n =   E.poolBandNotB E.li4 n -- as four bread cells plug into breedCells
                                        putStrLn "Enter: 0 to close"
                                        let loop = do
                                             
                                           --  miIO <- getLine
                                          --   foB <- getLine
                                          --   kk <- getLine
                                             out <- getLine
                                            -- let gonz some = read some 
                                             let guesseS n = E.poolBandNotB E.li4 n -- as four bread cells plug into breedCells
                                            -- let lab io a b guesseS = breedCELLs io a b guesseS  -- write guesses into a cell system
                                          --   bios <- forM [1..9] (\bio -> do
                                            -- lineN:Int, line of [abyssDevide2] , atom:Int, of lineN, atomGuess:Int,  
                                                  
                                                --  inner <- return (ratE 1 1 1)
                                            --      teest <- getLine 
                                               --   let ratE guess lineN atom = show (plugExpWORK  (map concat (map bootPunkt [1..6])) lineN guess (head$ausw atom (bootPunkt lineN))) 
                                              --    return (ratE bio teest 1 1))
                                            -- putStrLn $show bios  
                                           --  if show ion > ("50") then loop                      -- write Bayes
                                             if (read out)> 0 then do 
                                                            er <- getLine
                                                            at <- getLine
                                                           -- io <- getLine
                                                          --  let checkCell atomGuess randOm = ratE (read out) 1 1 1 -- (workPunktRAW [mother] (unline(bootPunkt 1)) "B" ["B"]  randOm )
                                                          --   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                                          --                        n   atom2    line2      rnd?     aGuess 
                                                            putStrLn $show (ratE [] (read out) (1) "B" (read out) 1) -- (read at))
                                                            putStrLn $show (ratE [] (2) (2) "BBBB" (read out) 2) -- (read at))
                                                            putStrLn $show (ratE [mother] (1) (3) "cell" (read out) 1) -- (read at))
                                                            putStrLn $show (ratEChars [] (1) (7) "cell" (read out) 3) -- (read at))
                                                            putStrLn $show (ratEChars [] (1) (7) "cell" (1) 3) -- (read at))

                                                            hu <- getLine 
                                                            putStrLn $show (ratE [mother] 1 1 hu 1 1) -- (read at))
                                                            putStrLn $show (ratE [mother] 2 3 ("cell"++hu) 1 1) -- (read at))
                                                            putStrLn $show (ratE [father] 2 2 hu 2 1) -- (read at))
                                                            putStrLn $show (ratE [mother2] 3 2 hu 3 1) -- (read at))
                                                            putStrLn $show (ratE [loopNumber] 4 2 hu 4 1) -- (read at))
                                                            putStrLn $show (ratE [minMaxTrueOrFalse] 5 2 hu 5 1) -- (read at))


                                                            loop
                                             else do putStrLn $"done"
                                        loop 
                                      -- line:Int ; select a String from abyssDevide2RAW    #############################  COULD SPLIT foli4 instead of                           
    {- MAIN BRANCH SET TO:       -}     let tailRAW line = (concat(ausw line abyssDevide2RAW))    -- #######   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                        putStrLn$ show$ map chr (guessWithBias 1)
                                        putStrLn$ show$ map chr (guessWithBias 2)
                                        putStrLn$ show$ map chr (guessWithBias 3)

                                        putStrLn$ show$ map concat ((map bootPunkt [1..6]))  --(type1or2 "ddd" 1 1)
                                        putStrLn$show$(workPunktRAW [mother] (unwords (bootPunkt 1 )) "B" ( abyssDevide2) 1 )
                                        putStrLn$show$(workPunktRAW [mother] (head(bootPunkt 2 )) "B" (head(ausw 1(ausw 1 abyssDevide2RAW))) 1 )
                                        putStrLn$show$(workPunktRAW [mother]  (concat (bootPunkt 3 )) "J" (head(ausw 1(ausw 3 abyssDevide2RAW))) 1 )
                                        putStrLn "WROTE new cells:"
                                        putStrLn$ show$ map concat ((map bootPunkt [1..6]))  --(type1or2 "ddd" 1 1)
                                       -- let cellAbyss2 =   -- what exactly put in 
                                        let oneCellAbyss2 io a sixBreadCells n = map (mapWithAbyss2 io a sixBreadCells) (tailRAW n) 
                                        putStrLn $ show $ tailRAW 1
                                        putStrLn$ show $ (oneCellAbyss2 [] "B1456.345" ["B1.23,4"] 1 )--(abyssDevide2)) 
                                        putStrLn$ show $ (oneCellAbyss2 [mother] "B123" ["B"] 1 )--(abyssDevide2))
                                        putStrLn$ show $ (oneCellAbyss2 [mother] "B" ["B"] 1 )--(abyssDevide2))
                                        putStrLn$ show $ (oneCellAbyss2 [mother] "F" ["B"] 2 )--(abyssDevide2))
--      CHAR LEVEL b                 ______ if [] => output triggerword "cell"++rndInt  OR [mother] .. from ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"] <-> [abyssDevide2] <-> [String]
--  given b is                     |       
--  e.g "B"                        |    no influence         
--  test ANY b                     |       |    ______  b:  if  b == abyssDevide2  then  ==> 0  if e.g b =="B123" abyssDevide =="BBBDF" then "B" 
--                                 |       |    |
--                (oneCellAbyss2 [mother] "B" ["B"] 1 ) -- select String of [abyssDevide2] 
--                                         |      \
--   STRING LEVEL (oneCellAbyss2 [mother] "JLLNP" (concat (ausw 3 abyssDevide2RAW)) 3) -- select String -> select Char => compare with b 
--  given b is 
{- e.g "JLLNP"         -}               putStrLn$ show $ (oneCellAbyss2 [loopNumber] "JLLNP" (concat (ausw 3 abyssDevide2RAW)) 3) -- ["J"] )--(abyssDevide2)) done
{- test ANY letter of b -}              putStrLn$ show $ (oneCellAbyss2 [loopNumber] "JLLNP" (concat (ausw 2 abyssDevide2RAW)) 3) -- ["J"] )--(abyssDevide2)) 
{- with abyssDevide2   -}               putStrLn$ show $ (oneCellAbyss2 [loopNumber] "JLLNP" (concat (ausw 3 abyssDevide2RAW)) 2) -- ["J"] )--(abyssDevide2)) 
                                        putStrLn$ show $ (oneCellAbyss2 [loopNumber] "JLLNP" (concat (ausw 2 abyssDevide2RAW)) 2) -- ["J"] )--(abyssDevide2)) 
                                        putStrLn$ show $ (oneCellAbyss2 [mother2] "JLLNP" (concat (ausw 3 abyssDevide2RAW)) 3) -- ["J"] )--(abyssDevide2)) 
                                        putStrLn$ show $ (oneCellAbyss2 [mother2] "1111" (concat (ausw 3 abyssDevide2RAW)) 3) -- ["J"] )--(abyssDevide2)) 
                                        putStrLn$ show $ (oneCellAbyss2 [mother2] "Z" (concat (ausw 3 abyssDevide2RAW)) 3) -- ["J"] )--(abyssDevide2))
                                  -- singularitY a big number as string e.g "123123123"
                                        let abyssDevide2TEST singularitY = let step1 =  concat $map guessWithBias $ map read (tails singularitY \\ [""])-- (usage 2)  
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]
                                        let iterAby =  let step1 =    map (map chr)  (abyssDevide2TEST "12341234") -- (usage 2)  
                                                          in let step2 =   (length step1) `div` 6  
                                                          in let toRead =   if step2==0 then 1 
                                                                            else step2 
                                                          in let switch a  =  (drop ((a-1)*6)) $ (take (a*6)) step1 
                                                          in map switch [1..(toRead)]

         --     write random things to cell  -> take a mapWithAbyss run -> filter the first two digits from every cell
                                                 -- stream: bootscells; charN: Int to a different matrix set a filled matrix1 with 1
                                        let cellLogic  line stream  = map (take 3 )(concat  (ausw line( (stream)))) 
                                        --toHeaven aby = map foHeaven [1..(length aby]
                                                                                    --   boot CELL I ..IV                ->   boot cells
                    -- 1. io: PunktIO , which ??? ; a:String compare a to b not used but with bayseian
                    --   forBreadCells: Int , select booted cell n of [abyssDevide]  ==  e.g  (concat (ausw 3 abyssDevide2RAW))
                    -- 2.  atom: Int , of ; line:Int , of random run 
                    --    
                    --
                    --    join uploaded cells with random Strings  
                    --     abyssDevide2RAW -> oneCellAbyss2 -> 'celLogic' ->   'littleWriter2'    <-    iterAby ( INSERT INT PROGRAM) <- abyssDevide2TEST <- guessWithBias  
                                        let littleWriter2 io a b forBreadCells n atom = let choose pick2 pick1 = (ausw pick1 (concat (ausw pick2 iterAby))) 
                                                                                        in let bootcells forBreadCells k = (mapWithAbyss2 io a forBreadCells k) 
                                                                                        in let celLogic io atomN line = (oneCellAbyss2 io "Z" (concat (ausw atomN abyssDevide2RAW)) line) --cellLogic  1 3 (oneCellAbyss2 io "JLLNP" (bootPunkt (read n)) line)
                                                                                        in let steps foatom = (cellLogic foatom (celLogic io atom (read n))) -- ######################### EXPORT 
                                                                 --     in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat (usage line))] 1 "dddd" "CELLONE" ) ) 
                                                                                        in  map concat$nub$(transpose) [(concat(map steps [1..6])),  (concat( (ausw (read n )iterAby)))]  --writeRNDcellInput (6) (read n) -- zipWith (++) (steps (read n)) (concat(ausw atom (ausw (read n )iterAby)))

            -- compare booted cells == bootPunkt with guesses, random Numbers in cells and foli4
                                        let compareCells io a b forBreadCells n atom = let choose pick2 pick1 = (ausw pick1 (concat (ausw pick2 iterAby))) 
                                                                                        in let bootcells forBreadCells k = (mapWithAbyss2 io a forBreadCells k) 
                                                                                        in let celLogic io atomN line = (oneCellAbyss2 io "Z" (concat (ausw atomN abyssDevide2RAW)) line) --cellLogic  1 3 (oneCellAbyss2 io "JLLNP" (bootPunkt (read n)) line)
                                                                                        in let steps foatom = (cellLogic foatom (celLogic io atom (read n))) -- ######################### EXPORT 
                                                                 --     in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",(concat (usage line))] 1 "dddd" "CELLONE" ) ) 
                                                                                        in let writeRNDcellInput atomN line = (plugExpWORK   ["cell","CELLONE","CELLTWO","CELLTHREE","CELLFOUR",concat(steps line )] 1 "dddd" "CELLONE" (head (bootPunkt line)) )
                                                                                        in let type1or2 guess lineN atom = (plugExpWORK  (map concat(map bootPunkt [1..6])) 1 guess (head$ausw atom (bootPunkt lineN)) (concat(steps lineN))) 
                                                                                        in  (type1or2 (concat(ausw (read n) foli4)) (read n) atom)
                                        putStrLn$show$(littleWriter2 [mother] "F" " F" ((ausw 3(concat (ausw 1 abyssDevide2RAW)))) "1" 1)                                          
                                        putStrLn $ (unwords(bootPunkt 3 ))
                                        putStrLn $ (concat(bootPunkt 3 ))
                                        putStrLn$(show$concat$(bootPunkt 3))
                                        putStrLn $show$ (workSlotRAW "B" "B" [])
                                        putStrLn $show$ (workSlotRAW "B1" "B" [mother])
                                        putStrLn $show$ (workSlotRAW "B111" "B" [mother])
                                        putStrLn $show$ (workSlotRAW "B" "B11" [mother])
                                        putStrLn $show$ (workSlotRAW "cell" "B11" [mother])
                                        putStrLn $show$ (workSlotRAW "cell" "cell" [mother])
                                        putStrLn $show$ (workSlotRAW "cell" "cellB" [mother])
                                        putStrLn $show$ (workSlotRAW "cellB" "cell" [mother])
                                        putStrLn $show$ (workSlotRAW "cellB" "cell" [mother])
                                        putStrLn $show$ (workSlotRAW "CELLONE" "cell" [father])
                                        putStrLn $show$ (workSlotRAW "CELLONE" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "CELLONE222" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "CELLONE" "CELLONE111" [father])
                                        putStrLn $show$ (workSlotRAW "C" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "K" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "CELLONEcell" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "C1" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "1C" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "1" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "CELLONEcell" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "1" "CELLONE" [father])
                                        putStrLn $show$ (workSlotRAW "J" "J11" [mother])
                                        putStrLn $show$ (workSlotRAW "X1" "J1" [father])
                                        putStrLn $show$ (workSlotRAW "X1" "X1" [father])

                                        putStrLn $show$ (workSlotRAW "B" "B11" [mother])
                                        putStrLn $ show  $ takeRndChars 1
                                        putStrLn $ show  $ takeRndChars 2
                                        putStrLn $ show  $ takeRndChars 3
                                        putStrLn $show$ map (map chr) (runRandomLin foli4)
                                        putStrLn $show $ newLayerRAW 1
                                        putStrLn$show $newLayerRAW 1
                                        putStrLn$ show$ map (map chr) (abyssDevide2TEST "12341234")
                                        putStrLn$show $(iterAby)
                                     --   let boa = do
                                       --    io <- getLine                   --   ["BBBDDF","FFHHJJ","JLLNNP","PPRRTT","TVVXXX","ZZZ\\\\^"]
                                        putStrLn$show$(littleWriter2 [] "aaa" "1" (concat (ausw 4 abyssDevide2RAW)) "1" 1) 
                                        putStrLn$show$(littleWriter2 [] "aaa" "1" ((ausw 1(concat (ausw 1 abyssDevide2RAW)))) "2" 1)
                                        putStrLn$show$(littleWriter2 [mother] "aaa" " B" ((ausw 1(concat (ausw 1 abyssDevide2RAW)))) "1" 1) 
                                        putStrLn$show$(littleWriter2 [loopNumber] "aaa" "B" ((ausw 4(concat (ausw 4 abyssDevide2RAW)))) "1" 2) 
                                        putStrLn$show$(littleWriter2 [mother2] "nothing" " F" ((ausw 3(concat (ausw 3 abyssDevide2RAW)))) "3" 3)
                                        putStrLn$show$(littleWriter2 [mother] "nothing" " J" ((ausw 1(concat (ausw 3 abyssDevide2RAW)))) "3" 1)
                                        putStrLn$show$(littleWriter2 [loopNumber] "nothing" "P" ((ausw 1(concat (ausw 4 abyssDevide2RAW)))) "4" 1)
                                        putStrLn$show$(littleWriter2 [father] "nothing" " J" (((concat (ausw 3 abyssDevide2RAW)))) "3" 2)

                                        putStrLn$show$head(ausw 1(concat(ausw 1 abyssDevide2RAW))) 
        -- CHOOSE EXPORT:                                                     make new cells                                                       compare cells
                                        let choosF n = head$ ausw n [ show (oneCellAbyss2 [mother2] "Z" (concat (ausw 3 abyssDevide2RAW)) 3),show(littleWriter2 [mother] "nothing" "J" (((concat (ausw 3 abyssDevide2RAW)))) "3" 2),show(type1or2 (unlines ["ddd"]) 1 1),show(workPunktRAW [mother] (head(bootPunkt 2 )) "B" (head(ausw 1(ausw 1 abyssDevide2RAW))) 1 ){-get a cell (char) of one selected adyssDevide-}] 
          -- select with selexport:Int
                                        return (BT.formationB [((choosF 1))])

    
                                        --boa --littleWriter2 io atom a forBreadCells n b                                  
                                       -- aTree
                                       -- putStrLn $show$ (workSlotRAW "X1" "J" [father])








                                        --        -> if breedCELLs [father] "B1" "B1" (abyssDevide2)  == 0 
                                        --    AND
                                        --          if simval match good write data 
                                        --  *HoofdDev> BT.bayesAstigmatic "cell" "B1"
                                        --  "B1"

  {-  


                                           -}
                                        --putStrLn (show(plugExp 3))


   where
      palette = runEXP3 foli4 stringBT pi ghADD; 
      choosDomain ff =  if ff == 1 then foli4 
                                   else stringBT; 
      chD ff  = choosDomain ff;
-- slotDomain:String, map a li; testMap:String, map various ghADD -- write to stringBT ? 
      rulerBT slotDomain testMap = baysianTypeString [""] slotDomain testMap;  -- test with right length the order of ghADD 
      protractorBT slotDomain strIntBT testMap = baysianTypeInt ["48","","","","","57"] slotDomain strIntBT testMap;
 -- map with filterB 
      actie pv1 pv3 pv4 pv6 foli ghAdd = inActie (pv1 pv3 pv4 pv6 foli [ghAdd]);
      filterB = map (filter (/='A')) E.li4;
      solong b = [1..(length b)];
 --which:Int length output; m: source list; fromA: Int choose from m
      soSame  which m fromA = let stap1 = head(ausw fromA m)
                 in take which [stap1,stap1..];
      bayesMonad b thisAction = do 
          let solang = solong b
          bayWorld0 <- forM solang (\bay -> do
              let outBayesMonad = thisAction
              return (outBayesMonad))
          bayWorld0; 
      typeCheckTriggerword = if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "intern" ) == "1" then palette 
                              else if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "cell" )  == "1" then (map read [(triggerWord "cell" "cell3")]) -- snd (ghAdd,(snd(charFilterInt ghAdd)))

                         else palette --(head (snd allFor))


                   


