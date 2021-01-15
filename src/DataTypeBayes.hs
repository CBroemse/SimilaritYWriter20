module DataTypeBayes(
   basis4 -- store data in Baysian string   
 , checkType  -- how useful ? 
 , formationB -- cell1 , cell2 with ptc9 ? 
 , nameBayesTypeString -- is liSolutions
 , nameBayesTypeInt  -- help to name bayes types with Ints
 , nameBayesDomain -- most basic bayes metric so far
  ) where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO

import UsefulFunctions19
import DataTypePunkt
---------------------------------------------------------
--friend front-end
-- stringBT: String, which name to remember as liSolution(BayesType String)
-- see example 'triggerWord' below
-- build single standing or cells on top of 'triggerWord'
nameBayesTypeString g domain stringBT = triggerWordRAW g domain stringBT
-- function above names the cells this function
-- filters an Int as type recognition initiation of baysian cell types I..IV
nameBayesTypeInt g domain stringBT = triggerWordRAW g domain stringBT


-- foAli: String the name of this Punkt (baas)
-- underlaying metric of this bayes type
nameBayesDomain foA foAli  =  (unwords(checkBayes [] [(Punkt foAli foA foA foA foA foA)] ))




---------------------------------------------------------
-- BELOW ALL BASIC DEEP BACK-END
-- basic needls
ausw l t = (drop(l-1) (take l t))
scanChar :: Char -> Int

scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

--scanString :: String -> Int a -> Double 
scanString = let ste = (go 0) 
             in zipWith (/) [1] 
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x
-- import your functions into kArmTest5 via formation
preX x = head(map scanChar (frmDoubletoInt (show x)))

    --putStrLn "Wrote Punkt: startWert"
---------------------------------------------------------------------------

frmDoubletoInt das = let st1 = map ord das
                 in let st2 = break (==46) st1 --(e.g [55,32,54] -> ([55],[32,54] ))
                 in let checkDigitBEFRkommata = length (fst st2) -- $break(==32)$map ord$das
                 in let cDBk = checkDigitBEFRkommata
                 in let toInt = map scanChar (show cDBk)
                 in if cDBk == 0 then map chr (filter (/=46) (snd st2))
                    else if cDBk == 1 then map chr ((fst st2))
                    else map chr ((fst st2)   )
basis2 foAL m = maybePu (head (ausw m foAL ))         -- ACCESS functions to any bonelist line 
basis3 foAL =  ((map ( (basis2 foAL)) [1..(length foAL)]))
basis2New foAL x m = maybePu (head (ausw m (foAL x))) 
-- Upload functions and apply them to the data
-- -- when buiding the final 'Punkt' structure these are different
-- computation that can be used with '[father,mother...' 
-- a) turn bonelist into Punkt
-- b) turn pg functions into Punkt via 'uniqueClassRAW' 
formationB e = Punkt "formation" (Just (basis2 e 1 )) (Just (basis2 e 2 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) 

-- store data in String because there are only 5 other spots left
-- when reading a longer list that wont help thus store in Punkt "String"
basis4 foAL r = maybePu (show(checkBayes [] [((basis2 foAL r))]))

-- baysian data
-- based on Punkt type 
--addGh:Int ; 1 == add new line to a bonelist: ghCheck and write111111111
-- e.g> let li = ["AAABB","AABAB","AAA","BBBAA"]
--       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
-- e.g>  kArmTest5 2 ["AAABB","AAABBAAABAB","AAABAB","AAA","AAABBBAA","BBBAA"] li 1 pi 1 1 [] "DD"
baysianRAW addGh liT bonelist mofaList connectWrist dit dit2 mCommand crit fourierMQ6NOPAN123 = do
     let allAcc foPun =  (checkBayes [] [(foPun)])
 
     let foAdecide foA = if foA==[] then Nothing
                         else (Just (maybePu foA)) 

-- make a function that is a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt 
-- this function below shall lead to => a motherTYPE that is depending on the type of simiyritYvalue
     let foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                          in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                                 in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                          in let preMoa = length foA
                          in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                          in 
                          if foA==[] then Nothing
                          else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                               in Just (show[(chssd 1)])
     let mayer foa = if foa == [] then [""]
                              else lines(show [(foAdecide2 foa)])
     let mayer2 r foa = if (foa) == [] then maybePu "empty"
                                       else maybePu2 (r) (Just(maybePu ((show [(foAdecide2 (foa))]))))
     let justNames = ["name1","name2","name3"]
     let motherType foas r = map (mayer2 (head(ausw r (justNames)))) ([foas])

     putStrLn "TEST mayer2"
     let formTestRAW io r e e2 normalFunction =  let punktOrPg = checkBayes  io [(Punkt  (head(checkBayes[] [(basis4 e2 r)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                 in let choosBy = length(head (group(punktOrPg)))
                 in if choosBy ==2 then normalFunction
                                   else punktOrPg 
 

     let formTest io r e normalFunction = formTestRAW io r e liT normalFunction

     let fnACCRAW cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
            where 
             unPoint = (show(head(words(unwords(checkBayes [] [connectWrist]))))) ;
 
 -- import 'roaming' data into kArmTrack5     |  
 --  all data in one clunky data-type :
     let checkFo g = if (length g) == 0 then ""
                     else "MOTHER MODE: on"  

     let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc (connectWrist))) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA
    -- EXAMPLE Punkt new format connect two [Maybe Punkt] list1 and list2
     let formationRAW io1 io2 rd = (head(head(map words(checkBayes io1 [(basis (checkBayes io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]))))
     let formation io2 rd = [(formationRAW [mother] io2 (realToFrac rd))] --(preX rd) list1 (checkflow [] [(basis4 list2 rd)])) 
--formTest io  rd bonelist io2 rd
   --  let formation2 io1 io2 rd  = checkflow [] [(basis (checkflow io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(F.fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]
     let asDot2 inp = ((map mayer (map words inp)))

 -- RETRIEVE the DATA turn any 
 -- type:  [Maybe Punkt] -> String
 -- with brute force.
     let justGoneRAW2 io t  = let prep1 = ((map ord (((head(ausw t((formation io t))))))))
         -- take always second
                           in let prep2 = head ((ord '[') `elemIndices` (prep1))
         -- take always second , fixed
                           in let prep3 = head  ((ord ']') `elemIndices` (prep1))

                           in let prep4 = drop prep2 (take prep3 ((head((ausw t ((formation io t))))))) 
                           in let filAll = filter (/=92) (filter (/=34) (filter (/=93) (filter (/=91) (map ord (prep4))))) 
                           in map chr filAll
     let justGone2 io t = justGoneRAW2 io t  --(asDot2 ([(show(formation [] 10))])) 
     putStrLn (show(map scanChar(formationRAW [mother] [mother] (realToFrac 1))))  --------------------------------------Punkt data: show pg values
--baysian pure code
----------------------------------------------------------------------------------
--see the different states of flowState above
-- e.g checkBayes [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkBayes lis f = let ste1 lis f= publishPunkt f lis
                   in let ste2 = map (ste1 lis) f
                   in ste2

---------------------------------------------------------------------------
-- transform any PUNKT conctruction into a string
-- Punkt a -> String a -> show a 
--architect: Punkt ; a whole Overview of the DataType tree with all its Branches
-- searchTrail: ACCESFUNCTIONS e.g. [mother,mother,father] 
publishPunkt architect searchTrail = let firstAccess = mysee architect searchTrail 
               in let ste1 = map ord (show firstAccess)
               in let breakCriteria = (32 `elemIndices` ste1)
     -- leersteln komme nur vor wenn ein term wie "Just blue" appears 
     -- this in order ot get rid of the Just
               in let findeLeerstelln = snd (break (==32) ste1)
               in let seeIfBreaksNeeded = if (length breakCriteria<=0) then ste1
                                          else findeLeerstelln 
               in let ste2 = filter (/=34) seeIfBreaksNeeded
               in  let mapToChar = map chr ste2
               in mapToChar

--------------------------------------------------
-- this builds our sheep family tree
-- HYRACHY IN DATA-TYPE
--          |ACCESS FUNCTIONS  
-- ___________|___________________________________________________________________________|
-- LEVEL 0   :|    nACC                                                                   |
-- LEVEL 1 in:|    nACCRAW -> fst -> tracker1                                             |
--            |            -> snd -> findArm,findHand                                     |
-- LEVEL 1    |     tracker1        |    findArm,findHand                                 |   
-- LEVEL 1 output =>fst: "tracker!" OR    snd:  ["findArm","hand"])                       |                      
-- LEVEL 2    |                     |    innerAccessRAW "1" this OR inner..RAW "2" this   |
-- LEVEL 2    |   e.g 
-- -- as INNERACCESS function retieving data of each single ACCESSFUNCTIONS          
-- within the string of a Punkt
-- e.g: *Main> Co.innerAccessRAW "2" (unwords ["findArm","hand\n"])
--      " hand\n"
innerAccessRAW on this = let convToBit = map ord this
                   in let chopUp1 = (break (<=32) convToBit)
                   in let chopUp2 = (break (>=32) convToBit)

                   in if on=="1" then 
                        --let theFst = fst (break (>32) ((fst chopUp1)))	
                        map chr (fst chopUp1)
                      else
                        (map chr (snd chopUp1))

innerAccess on this = (innerAccessRAW on) this
----------------------------------------------------------------------
-----------------------------------------------------------------------
--fill 'AccesFuctions' with data
-- theName: String; Fill AccesFuctions with Data
-- set to show tracker1 see below
nameACCESSFUNCTION on theName input= let whichAccesFunction = (innerAccess on theName)
                            in let theData = whichAccesFunction++" "++(unlines input)
                            in theData


--short
nACC theName input = (nameACCESSFUNCTION "1" theName input)
nACCRAW tracker1 input = break (==' ') (nACC tracker1 input)
--nACCTAG =  
---------------------------------------------------------------------
----ACCESS FUNCTIONS-------------------------------------------------
-- getFunctionByName "father" = father
-- getFunctionByName "mother" = mother
-- getFunctionByName "mother2" = mother2
-- getFunctionByName "loopNumber" = loopNumber
-- getFunctionByName "minMaxTrueOrFalse" = minMaxTrueOrFalse
accesFWX l nlist = let aw1 n = (ausw n nlist)
                 in let wielanGg  = [1..l]
                 in  ([wielanGg ,[(l+1)..(l*2) ]])


checkType ffi = [(map length (group(sort(ffi)))), (ffi)]


-------------------- 15-1-21
--develop a cell and give order to it
-- or Bayes board 
---------------------------------------------------------------
--access intern or extern via Punkt ..."intern" ...
--
startDchange g t iOE = let piToBinary = nameBayesDomain Nothing t  
                       in if g == t then fst iOE --fst internal or external
                          else snd iOE
triggerWord g domain = triggerWordRAW g domain "cell"
triggerWordRAW g domain focell = if domain == "intern" then show(stC1 g )
                       else if domain == domain then (stC2 g domain) -- "cell" 
                       else show(stC1 g)
     where
        stC1 g = (startDchange g "intern" (1,2));
   -- get an in from idea name ("cell" ++ (show token))
   -- this prep step depends on another A'' set 
   -- pv1: a b c d e f
   -- pv2: g h i j k l
   -- pv3: m n o p q r
   -- pv4: s t u v w x________________  
   --                 |missing 'y z' |
   --                 | and   '='    |
   --                 |              |
   --                 |______________|
   -- y :: a..z \\ (z-1)
        fromCell = map chr [48..57];
        toHaal toInt =  charFilterInt toInt;
        condI numP   = if (fst (toHaal numP ))== focell && (snd (toHaal numP)) >= 0 then (toHaal numP)
                       else if (numP) == (focell++"3") then (numP,(snd(charFilterInt numP)))
                       else ((unlines(checkBayes [mother] [(maybePu "CELL")])) , 0 ) ;
        stC2 g numP = startDchange g  (fst(condI numP)) (show(snd(condI numP)),(fst(condI numP)));


