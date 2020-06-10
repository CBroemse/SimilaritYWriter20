# SimilaritYWriter20

A function that can compare any two values:

--------------------------------------------------------------------------
Gaussian influenced - Aehnlichkeitswarscheinlichkeit
this function rates the similarity of two
lists compares thme und gives back a percentage of the "Einerstelle"

=> foaehnli1 a b = (a-b)
=> foaehnli2 a b = (b-a)

turn into  % with a < b and b > a  

=>aehnlichF a b = let a1 = if a > b then ((foaehnli1 a b)/ (a/100) )
=>                       else if a<b then  ((foaehnli2 a b)/ (b/100) )
=>                       else 0
=>               in let b1 g h = ((g) / ((h)/100)) 
=>               in a1 

 
=> similaritYvalue li la = let a = la -- z.b. [11,12,34,44]
=>                      in let b = li 
=>                      in let c1 w = sum w
=>                      in let c2 = c1 a--length
=>                      in let c3 = c1 b--length
=>                      in  aehnlichF c2 c3 -- in let d = 

