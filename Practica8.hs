class Package p where
 handle :: (a -> b) -> p a -> p b

instance Package Maybe where
 handle f Nothing = Nothing
 handle f (Just x) = Just (f x)

instance Package List where
 handle = map 

