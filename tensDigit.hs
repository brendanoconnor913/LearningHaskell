tensDigit :: Integral a => a -> a

tensDigit x = d
    where xLast = fst $ divMod x     10 
          d     = snd $ divMod xLast 10

hunsD :: Integral a => a -> a

hunsD x = d
    where d2 = fst $ divMod x  100
          d  = snd $ divMod d2 10 
