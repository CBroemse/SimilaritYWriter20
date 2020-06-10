# SimilaritYWriter20

###### Index:   
-  to describe certain Types we wil use ' 

            :=' -- which is equivalent to '::' but ':=' /= '::' thus

                  

A function that can compare any two lists:

     this will be the main example being used throughout this program
     
     *> let li = ["AAABB","AABAA","AAA","BBBAA"]
     *> let bonelist = li
     *> let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing  -- "notM" will yield other output
     
     -- simiariYvalue network
     *> kArmTest5 li 1 pi 1 1 [] "AAA"    -- the last item "AAA" will be the test run
 
     we compare all lines and atoms of the 
     
     kArmTest5 (bonelist) ::= type [ord (String)] -> [[Int]] -> [[Double]] => [([String],[Double])]
     
     The aim is to find the most general rules that can 'maybe' 
     help our understanding. 11


######  STEP 1: Examine

There is an syntactic level and a conceptual level

    =======================================================================================================================
                                  SYNTAX                  |             CONCEPT
    =======================================================================================================================
       hints:       all Functions in 'e.g-sections' have  |  The 'e.g-sections' should be able to be called via ghc
                    to be takenout of the do functions    |  without a doupt that shall help understanding.
                      in 'chainDistribute'    AND         |  see if atom is already in list , dile:
                         'kArmTest5'                      |
    ------------------------------------------------------------------------------------------------------------------------
    given:  row-width (length atom bonelist, 'zeilenlaenge ) 
            pick1:String; a of (a,b) -> compare a to b    | one feasible way is to 'weigh' Strings is with simiaritYvalue
            pick2:String; b of (a,b) -> compare a to b    | The aim is to rerieve a reliable function that will destinct 
                                                          | between any two strings. 
          via ( beRepKEY pick1 pick2 punktList )          |           punktist :: Punkt -> [Punkt] -> not used
                                                                         ausw pick1 bonelist ->  atombonelist 
                                                                           

    e.g Colored*>  ( beRepKEY "1" "2" [] )           

#####   ABSTRACT

######  Function:'kArmTest'

STEP 2: An abstact overview of the content of 'kArmTest5' in 'Colored_2_3_5_Counter20.hs'
with I . CHAIN DISTRIBUTION
     II. SIMILARITYVALUE 
         rate with simiYvalue to compare  all atoms of all lines to the same metric
         
the matrix alike structures below represent lists not matrices 
where   a a a a = line1 ; b b b b = line2 ... x x x x = line n 

    -------------------------------------------
          read  |   complement |  rate with                   
       bonelist |   fst ..last |  simiYvalue              
    ------------------------------------------------
      a a a a   |   a1 a2 a3   |    a a a     a01 a01 a01           |a0 b0 c0|           b0 a0 c0     
      b b b b  --\  b0 b2 b3 --\  b b b --\   a01 a01 a01 --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
      c c c c  --/  c0 c1 c3 --/  c c c --/   a3  a3  a3  --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/
      d d d d   |   d0 d1 d2   |  d d d       a2  a2  a2            |a4 b4 c4|           c3 a3 b2 
    ======================================================================================================
              |a0 b0 c0|           b0 a0 c0       
    --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
    --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/  0 0 0
    ========================================================


--------------------------------------------------------------------------
A function that can compare any two values:

Gaussian influenced - Aehnlichkeitswahrscheinlichkeit
this function rates the similarity of two
lists of the same list compares them und gives back a percentage of the "Einerstelle"

  ##### 'Colored_2_3_5_Countrer20':= C*
  
     C*> cond1 a b = (a-b)
      *> cond2 a b = (b-a)
      
 ##### 'similaritYvalue' ('src/Colered_2_3_5_Counter.hs')

*turn into  % with a < b and b > a  

    *>simiValF a b = let a1 = if a > b then ((cond1 a b)/ (a/100) )
                              else if a<b then  ((cond2 a b)/ (b/100) )
                              else 0
                     in let b1 g h = ((g) / ((h)/100)) 
                     in a1 

 
    *> similaritYvalue li la = let a = la -- e.g [11,12,34,44]
                          in let b = li 
                          in let c1 w = sum w
                          in let c2 = c1 a   -- length
                          in let c3 = c1 b--length
                          in simiValF c2 c3 


