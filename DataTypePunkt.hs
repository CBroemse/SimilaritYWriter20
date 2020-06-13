module DataTypePunkt
  where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO

--------------------------------------------------------------
--------------------------------------------------------------
--FARBGEBER KUGELN
--------------------------------------------------------------

bougaer x= let gaga =(read x)
           --in let step1 = counterDreierKugel x
           in let tessen = ((gaga)-1)  
           in let boui = stelle3er gaga --  if (charly x) == True then counterDreierKugel (show tessen)
                          --else step1
           in let step2 = if (boui) == 1 then show "green"
                          else if (boui) == 2 then show "red"
                          else  "blue" --if (boui) == 3 then show "blue"
                         -- else if (boui) == 4 then show"green"
                       --   else if (boui) == 5 then show "red"
                         -- else show "green"
--in let step3 = if step2 == 1 then  show "green"
  --                        else if step2 == 2 then show "red"
                  --        else show "blue" 
           in step2


bougaer2 x= let gaga =(read x)
           --in let step1 = counterDreierKugel x
            in let tessen = ((gaga)-1)  
            in let boui = stelle3er gaga --  if (charly x) == True then counterDreierKugel (show tessen)
                         
            in let step2 = if (boui) == 1 then 1
                           else if (boui) == 2 then 2
                           else  3 --if (boui) == 3 then show "blue"
                         
            in step2
-- 
-- Der Datentyp fuer die 2er,3er,5er listen
--  2er: type: 2 -> [1,2,] [3,4]... der Farbcode
--  3er : type: 3 -> [1,2,3] [4,5,6]...   "
--  5er. type 5 -> [1,2,3,4,5] [6,7,8,9,10] ...
--  wird in Farbgeber (baugaer) eingep
--  y = choose modus 2,3,5
--  x = input
arc t f = (t/f)

datenTypZahlen y x =( take y (concatMap (replicate x) [1..y]))



datenTypZahlen2 x f = let a = drop (f-1) (take f (concat (take f (repeat [1..x])))) 
              --in let listel  = last (take f a)
                     in a 
-- 

-- ---------------------------------------------------
-- ENTGUELTIGE FARBAUSGABE DER KUGELN
-- ----------------------------------------------------
-- choose modus s. oben 
plugit input chosemodus =    let a x = map bougaer (map show x)
                             in let b = (datenTypZahlen input chosemodus) 
                             in let bbeide = a b
                             in last bbeide
--------------------------------------------------------------
plugit2 input chosemodus =   let a x = map bougaer2 (map show x)
                             in let b = (datenTypZahlen input chosemodus) 
                             in let bbeide = a b
                             in last bbeide
--------------------------------------------------------------



--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- SPUREN 2ER 3ER 5ER
-- -----------------------------------------------------------
stelle2er f = let a = concat (take f (repeat [1,2]))
              in let listel  = last (take f a)
              in listel 


stelle3er f = let a = concat (take f (repeat [1,2,3]))
              in let listel  = last (take f a)
              in listel 

----------------------------------------------------
----------------------------------------------------



stelle5er f = let a = concat (take f (repeat [1,2,3,4,5])) 
              in let listel  = last (take f a)
              in listel 

--

--ghci> all (>4) [6,9,10]  
--      True 
dasSystem x = take 2 [1..x]


-- zaehlt in der 2er Spur und ordnet zahlen in gerade oder ungerade

counterZweier x = bo  
     where
             a= odd (read x);
             bo = if a == True then show "ungerade"
                  else show "Gerade"
{-
contar f = let a = [1,6..]
           in let b = [2,7..]
           in let c = [3,8..] 
           in let d = [4,9..]
           in let e = [5,10..]
           in let func x tz = (last (take x tz))
           in let listel x = [(func x a),(func x b),(func x c),(func x d),(func x e)]
           in listel f


--Zahl x 
--Zeile y
bert x y = let a=  x `elemIndices` y
           in if x == 0 then a
             else a 
          
--y = zahl
--
toSystem5 x y = let nu = contar x
                in let be = head (drop (y-1) (take y nu))
              --  in let zui = be [nu] 
                in  be -- zui
 
dhfg  x y = let boss = chr x
            in let boss2 = map chr y
            in  length (snd (break (>boss) boss2)) -- zui


-}
 

-------------------------------------------------------------------------------
--- ***************************************************************************
-- the POINT SYSTEM an old Type system starting with a clean slate.
-- 19.06.19
--
-- ............................................................................
--
data Punkt = Punkt {name::String, mother::Maybe Punkt, father::Maybe Punkt,mother2::Maybe Punkt,loopNumber:: Maybe Punkt,minMaxTrueOrFalse::Maybe Punkt}
-- Punkt "what" 
data PunktInt =  PunktInt {nameI::Int, motherI::Maybe Punkt, motherII::Maybe Punkt,motherIII::Maybe Punkt,motherIV:: Maybe Punkt,motherV::Maybe Punkt}



-- we show sheep by name
instance Show Punkt where
  show s = show (name s)
	

-- we show sheep by name
instance Show PunktInt where
  show s = show (nameI s)


-- traceFamily is a generic function to find an ancestor
richtungHoeheY :: Punkt -> [ (Punkt -> Maybe Punkt) ] -> Maybe Punkt
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
mysee s searchTrail = richtungHoeheY s searchTrail
-- this allows the user to name the mother and father functions on the command line
-- DATATYPE: Punkt; with all its 5 ACCESFUNCTIONS
getFunctionByName :: String -> (Punkt -> Maybe Punkt)
getFunctionByName "father" = father
getFunctionByName "mother" = mother
getFunctionByName "mother2" = mother2
getFunctionByName "loopNumber" = loopNumber
getFunctionByName "minMaxTrueOrFalse" = minMaxTrueOrFalse
getFunctionByName _        = error "Invalid function name - not 'mother' or 'father'"


