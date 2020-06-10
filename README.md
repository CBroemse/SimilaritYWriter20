# SimilaritYWriter20

A function that can compare any two lists:

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


--------------------------------------------------------------------------
A function that can compare any two value:

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


