class YesNo a where
  yesno :: a -> Bool
  yesyes :: a -> Bool -- lol

instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  yesyes _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True
  yesyes _ = True

instance YesNo Bool where
  yesno = id
  yesyes _ = True


maybeints = Just [1,2,3]
