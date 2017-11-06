newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem first) (Mem second) = Mem (\s -> ((mappend (getA s first) (getA s second)), snd (second (getS s first))))
    where getS s f = snd $ f s
          getA s f = fst $ f s
