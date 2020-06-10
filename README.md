# SimilaritYWriter20

A function that can compare any two values:

--------------------------------------------------------------------------
Gaussian influenced - Aehnlichkeitswarscheinlichkeit
this function rates the similarity of two
lists compares thme und gives back a percentage of the "Einerstelle"

  #####'Colored_2_3_5_Countrer20':= C*
  
     C*> cond1 a b = (a-b)
      *> cond2 a b = (b-a)
      
 ##### 'similaritYvalue' ('src/Colered_2_3_5_Counter.hs')

*turn into  % with a < b and b > a  

    *>simiValF a b = let a1 = if a > b then ((cond1 a b)/ (a/100) )
    *>                         else if a<b then  ((cond2 a b)/ (b/100) )
    *>                         else 0
    *>               in let b1 g h = ((g) / ((h)/100)) 
    *>               in a1 

 
    *> similaritYvalue li la = let a = la -- e.g [11,12,34,44]
    *>                      in let b = li 
    *>                      in let c1 w = sum w
    *>                      in let c2 = c1 a   -- length
    *>                      in let c3 = c1 b--length
    *>                      in simiValF c2 c3 

