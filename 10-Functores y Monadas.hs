class Package f where
 handle :: (a -> b) -> f a -> f b -- fmap

instance Package Maybe where
  handle f Nothing  = Nothing
  handle f (Just x) = Just (f x)

instance Package [] where
  handle = map

instance Package ((->) c) where
  handle = (.)

class Package m => Box m where
  unite :: Box m => m (m a) -> m a -- join
  pack :: a -> m a -- return

instance Box Maybe where
  unite Nothing  = Nothing
  unite (Just x) = x
  pack = Just

instance Box [] where
  unite  = concat
  pack x = [x]

instance Box ((->) c) where
  unite f x = f x x 
  pack = const

link :: Box m => (a -> m b) -> m a -> m b -- bind (>>=)
link f b = unite (handle f b)

-- Alternatively we can write handle with link and pack!
handle' :: Box m => (a -> b) -> m a -> m b -- liftM
handle' f p = link (pack . f) p

-- Alternatively we can write unite with link!
unite' :: Box m => m (m a) -> m a -- join
unite' p = link id p          -- link f m = join (fmap f m)

employ :: Box m =>  m (a -> b) -> m a -> m b -- ap
employ f x = link (\g -> handle' g x) f

associate :: Box m => (a -> m b) -> [a] -> m [b] -- mapM
associate f xs = succession (handle' f xs)

succession :: Box m => [m a] -> m [a] -- sequence
succession xs = associate id xs

pred3 x = do n <- pred x    -- pred3 x = pred x >>= \n ->
             m <- pred n    -- pred n >>= \m ->
             o <- pred m    -- pred m >>= \o ->
             return o       -- return o
  where pred Z = Nothing    --  where pred Z     = Nothing
        pred (S x) = Just x --        pred (S x) = Just x

guard :: Bool -> [()] 
guard True  = [()] -- return ()
guard False = []   -- mzero

-- Returns all suffixes of a given list
suffixes :: [Int] -> [[Int]]
suffixes []     = []
suffixes (x:xs) = (x:xs) : suffixes xs

-- Returns all the posible sublists that add up to k
subsetsum :: [Int] -> Int -> [[Int]]
subsetsum xs k =                   subsetsum' xs k =
  if k == 0 then                     if k == 0 then
    return []                          return []
  else do                            else do
    (x:xs') <- suffixes xs             suffixes xs             >>= \(x:xs') ->
    guard (x <= k)                     guard (x <= k)          >>= \_ ->
    ys <- subsetsum xs' (k-x)          subsetsum' xs' (k-x)    >>= \ys ->
    guard (sum (x:ys) == k)            guard (sum (x:ys) == k) >>= \_ ->
    return (x:ys)                      return (x:ys)

