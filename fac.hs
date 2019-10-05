k b a = (((fst a)+1), (snd b * fst a))
fac :: [(Integer,Integer)]
fac = scanl k (1,1) fac 
