-- | Designed for qualified import. Operations are strict.
module Cabal.Matrix.Rectangle
  ( Rectangle(..)
  , rows
  , indexRow
  , toRowMajor
  , mapRows
  , columns
  , indexColumn
  , toColumnMajor
  , mapColumns
  , indexCell

  , unitRow
  , empty
  , vertical

  , productRows
  , blockDiagonal
  , appendRowsUnioningColumns
  , subtractRowsBySubsetColumns
  , unCartesianProduct
  ) where

import Control.Monad.ST
import Data.Foldable
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Primitive
import Data.Set qualified as Set


-- | A rectangle is a collection of rows of type @a@, columns of type @b@, and
-- for each row and column pair a cell of type @c@.
data Rectangle a b c = Rectangle
  {-# UNPACK #-} !(Array a)
  {-# UNPACK #-} !(Array b)
  {-# UNPACK #-} !(Array c)
  -- ^ INVARIANT: length of the third array is the product of the lengths of the
  -- first two
  deriving stock (Show, Functor)

rows :: Rectangle a b c -> Array a
rows (Rectangle rs _ _) = rs

indexRow :: Rectangle a b c -> Int -> (a, [(b, c)])
indexRow (Rectangle rs cs cells) y =
  ( indexArray rs y
  , [ (indexArray cs x, indexArray cells (y * width + x))
    | x <- [0 .. width - 1]
    ]
  )
  where
    !width = sizeofArray cs

toRowMajor :: Rectangle a b c -> [(a, [(b, c)])]
toRowMajor rect@(Rectangle rs _ _)
  = [indexRow rect y | y <- [0 .. height - 1]]
  where
    !height = sizeofArray rs

mapRows :: (a -> a') -> Rectangle a b c -> Rectangle a' b c
mapRows f (Rectangle rs cs cells) = Rectangle (mapArray' f rs) cs cells

columns :: Rectangle a b c -> Array b
columns (Rectangle _ cs _) = cs

indexColumn :: Rectangle a b c -> Int -> (b, [(a, c)])
indexColumn (Rectangle rs cs cells) x =
  ( indexArray cs x
  , [ (indexArray rs y, indexArray cells (y * width + x))
    | y <- [0 .. height - 1]
    ]
  )
  where
    !width = sizeofArray cs
    !height = sizeofArray rs

toColumnMajor :: Rectangle a b c -> [(b, [(a, c)])]
toColumnMajor rect@(Rectangle _ cs _)
  = [indexColumn rect x | x <- [0 .. width - 1]]
  where
    !width = sizeofArray cs

mapColumns :: (b -> b') -> Rectangle a b c -> Rectangle a b' c
mapColumns f (Rectangle rs cs cells) = Rectangle rs (mapArray' f cs) cells

indexCell :: Rectangle a b c -> Int -> Int -> c
indexCell (Rectangle _ cs cells) x y = indexArray cells (y * width + x)
  where
    !width = sizeofArray cs

unitRow :: a -> Rectangle a b c
unitRow !x = Rectangle (arrayFromListN 1 [x]) emptyArray emptyArray

empty :: Rectangle a b c
empty = Rectangle emptyArray emptyArray emptyArray

vertical :: b -> [(a, c)] -> Rectangle a b c
vertical c pairs = Rectangle
  (arrayFromList $ fst <$> pairs)
  (arrayFromListN 1 [c])
  (arrayFromList $ snd <$> pairs)

-- | Applies the function strictly
productRows
  :: (a -> a' -> a'')
  -> Rectangle a b c
  -> Rectangle a' b c
  -> Rectangle a'' b c
productRows combine (Rectangle rs cs cells) (Rectangle rs' cs' cells') =
  Rectangle
    (createArray height'' (error "urk") \m -> do
      for_ [0 .. height - 1] \y -> do
        let !r = (indexArray rs y)
        for_ [0 .. height' - 1] \y' -> do
          writeArray m (y * height' + y')
            $! combine r (indexArray rs' y'))
    (cs <> cs')
    (createArray (width'' * height'') (error "urk") \m -> do
      for_ [0 .. height - 1] \y -> do
        for_ [0 .. height' - 1] \y' -> do
          copyArray m ((y * height' + y') * width'')
            cells (y * width) width
          copyArray m ((y * height' + y') * width'' + width)
            cells' (y' * width') width')
  where
    !width = sizeofArray cs
    !width' = sizeofArray cs'
    !width'' = width + width'
    !height = sizeofArray rs
    !height' = sizeofArray rs'
    !height'' = height * height'

blockDiagonal :: Rectangle a b c -> c -> c -> Rectangle a b c -> Rectangle a b c
blockDiagonal (Rectangle rs cs cells) !tl !br (Rectangle rs' cs' cells') =
  Rectangle
    (rs <> rs')
    (cs <> cs')
    (createArray (width'' * height'') (error "urk") \m -> do
      for_ [0 .. height - 1] \y -> do
        copyArray m (y * width'')
          cells (y * width) width
        for_ [0 .. width' - 1] \x' -> do
          writeArray m (y * width'' + width + x') tl
      for_ [0 .. height' - 1] \y' -> do
        for_ [0 .. width - 1] \x -> do
          writeArray m ((height + y') * width'' + x) br
        copyArray m ((height + y') * width'' + width)
          cells' (y' * width') width'
        )
  where
    !width = sizeofArray cs
    !width' = sizeofArray cs'
    !width'' = width + width'
    !height = sizeofArray rs
    !height' = sizeofArray rs'
    !height'' = height + height'

data Source b c
  = FromNeither !b !c
  | FromLeft {-# UNPACK #-} !Int !c
  | FromRight {-# UNPACK #-} !Int !c
  | FromBoth {-# UNPACK #-} !Int {-# UNPACK #-} !Int (b -> b -> b)

appendRowsWithPairing
  :: Array (Source b c) -> Rectangle a b c -> Rectangle a b c -> Rectangle a b c
appendRowsWithPairing pairing (Rectangle rs cs cells) (Rectangle rs' cs' cells')
  = Rectangle
    (rs <> rs')
    (flip mapArray' pairing \case
      FromNeither c _ -> c
      FromLeft x _ -> indexArray cs x
      FromRight x' _ -> indexArray cs' x'
      FromBoth x x' combine -> combine (indexArray cs x) (indexArray cs' x'))
    (createArray (width'' * height'') (error "urk") \m -> do
      for_ [0 .. height - 1] \y -> do
        for_ [0 .. width'' - 1] \x'' -> do
          writeArray m (y * width'' + x'')
            $! case indexArray pairing x'' of
              FromNeither _ cell -> cell
              FromLeft x _ -> indexArray cells (y * width + x)
              FromRight _ cell -> cell
              FromBoth x _ _ -> indexArray cells (y * width + x)
      for_ [0 .. height' - 1] \y' -> do
        for_ [0 .. width'' - 1] \x'' -> do
          writeArray m ((height + y') * width'' + x'')
            $! case indexArray pairing x'' of
              FromNeither _ cell -> cell
              FromLeft _ cell -> cell
              FromRight x' _ -> indexArray cells' (y' * width' + x')
              FromBoth _ x' _ -> indexArray cells' (y' * width' + x'))
  where
    !width = sizeofArray cs
    !width' = sizeofArray cs'
    !width'' = sizeofArray pairing
    !height = sizeofArray rs
    !height' = sizeofArray rs'
    !height'' = height + height'

makePairing :: Ord b => c -> Array b -> Array b -> Array (Source b c)
makePairing dflt left right = arrayFromList $ go initMap initSet 0
  where
    !leftSize = sizeofArray left
    !rightSize = sizeofArray right
    !initMap = Map.fromListWith IntSet.union
      [ (indexArray right i, IntSet.singleton i)
      | i <- [0 .. rightSize - 1]
      ]
    !initSet = IntSet.fromDistinctAscList [0 .. rightSize - 1]

    go !m !s !i
      | i >= leftSize = (`FromRight` dflt) <$> IntSet.toAscList s
      | !l <- indexArray left i
      , Just js <- Map.lookup l m
      , (j, js') <- IntSet.deleteFindMin js
      = FromBoth i j const : go
        (if IntSet.null js' then Map.delete l m else Map.insert l js' m)
        (IntSet.delete j s) (i + 1)
      | otherwise = FromLeft i dflt : go m s (i + 1)

appendRowsUnioningColumns
  :: Ord b => c -> Rectangle a b c -> Rectangle a b c -> Rectangle a b c
appendRowsUnioningColumns dflt left right = appendRowsWithPairing
  (makePairing dflt (columns left) (columns right)) left right

filterRowsBySelector
  :: Array Int -> (Array c -> Bool) -> Rectangle a b c -> Rectangle a b c
filterRowsBySelector selector predicate (Rectangle rs cs cells) = runST do
  mrs' <- newArray height (error "urk")
  mcells' <- newArray (width * height) (error "urk")
  let
    go !y !y'
      | y >= height
      = do
        rs' <- freezeArray mrs' 0 y'
        cells' <- freezeArray mcells' 0 (y' * width)
        pure $ Rectangle rs' cs cells'
      | predicate $ mapArray' (\x -> indexArray cells (y * width + x)) selector
      = do
        writeArray mrs' y' (indexArray rs y)
        copyArray mcells' (y' * width) cells (y * width) width
        go (y + 1) (y' + 1)
      | otherwise
      = go (y + 1) y'
  go 0 0
  where
    !width = sizeofArray cs
    !height = sizeofArray rs

makeSelector :: Ord b => Array b -> Array b -> Maybe (Array Int)
makeSelector left right = go initMap 0 []
  where
    !leftSize = sizeofArray left
    !rightSize = sizeofArray right
    !initMap = Map.fromListWith IntSet.union
      [ (indexArray left i, IntSet.singleton i)
      | i <- [0 .. leftSize - 1]
      ]

    go !m !i out
      | i >= rightSize = Just $ arrayFromListN rightSize $ reverse out
      | !r <- indexArray right i
      , Just js <- Map.lookup r m
      , (!j, !js') <- IntSet.deleteFindMin js
      = go
        (if IntSet.null js' then Map.delete r m else Map.insert r js' m)
        (i + 1) (j:out)
      | otherwise = Nothing

cellsRowMajorArr :: Rectangle a b c -> [Array c]
cellsRowMajorArr (Rectangle rs cs cells) =
  [ cloneArray cells (y * width) width
  | y <- [0 .. height - 1]
  ]
  where
    !width = sizeofArray cs
    !height = sizeofArray rs

subtractRowsBySubsetColumns
  :: (Ord b, Ord c) => Rectangle a b c -> Rectangle a' b c -> Rectangle a b c
subtractRowsBySubsetColumns !rect !rect'
  = case makeSelector (columns rect) (columns rect') of
    Nothing -> rect
    Just selector
      | !rowSet <- Set.fromList $ cellsRowMajorArr rect'
      -> filterRowsBySelector selector (`Set.notMember` rowSet) rect

partitionByBitmaskAtOffset :: Array Bool -> Array a -> Int -> (Array a, Array a)
partitionByBitmaskAtOffset !bitmask !input !offset = runST do
  mfalses <- newArray len (error "urk")
  mtrues <- newArray len (error "urk")
  let
    go !f !t !i
      | i >= len = (,)
        <$> freezeArray mfalses 0 f
        <*> freezeArray mtrues 0 t
      | indexArray bitmask i = do
        writeArray mtrues t $ indexArray input (offset + i)
        go f (t + 1) (i + 1)
      | otherwise = do
        writeArray mfalses f $ indexArray input (offset + i)
        go (f + 1) t (i + 1)
  go 0 0 0
  where
    !len = sizeofArray bitmask

unCartesianProduct
  :: Ord c
  => Array Bool
  -> Rectangle a b c
  -> (Rectangle () b c, Rectangle () b c, Rectangle () () (Maybe Int))
unCartesianProduct bitmask (Rectangle rs cs cells) = runST do
  mfalses <- newArray (fwidth * height) (error "urk")
  mtrues <- newArray (twidth * height) (error "urk")
  mfis <- newPrimArray height
  mtis <- newPrimArray height
  let
    go !y !fMap !tMap
      | y >= height = do
        fcells <- freezeArray mfalses 0 (fwidth * fSize)
        tcells <- freezeArray mtrues 0 (twidth * tSize)
        let !frs = createArray fSize () \_ -> pure ()
        let !trs = createArray tSize () \_ -> pure ()
        icells <- do
          m <- newArray (fSize * tSize) Nothing
          for_ [0 .. height - 1] \i -> do
            fi <- readPrimArray mfis i
            ti <- readPrimArray mtis i
            writeArray m (ti * fSize + fi) (Just i)
          unsafeFreezeArray m
        pure
          ( Rectangle
            frs
            fcs
            fcells
          , Rectangle
            trs
            tcs
            tcells
          , Rectangle
            trs
            frs
            icells
          )
      | (!fc, !tc) <- partitionByBitmaskAtOffset bitmask cells (y * width)
      = do
        !fMap' <- case Map.lookup fc fMap of
          Nothing -> do
            copyArray mfalses (fSize * fwidth) fc 0 fwidth
            writePrimArray mfis y fSize
            pure $ Map.insert fc fSize fMap
          Just i -> do
            writePrimArray mfis y i
            pure fMap
        !tMap' <- case Map.lookup tc tMap of
          Nothing -> do
            copyArray mtrues (tSize * twidth) tc 0 twidth
            writePrimArray mtis y tSize
            pure $ Map.insert tc tSize tMap
          Just i -> do
            writePrimArray mtis y i
            pure tMap
        go (y + 1) fMap' tMap'
      where
        !fSize = Map.size fMap
        !tSize = Map.size tMap
  go 0 Map.empty Map.empty
  where
    !width = sizeofArray cs
    !height = sizeofArray rs
    (!fcs, !tcs) = partitionByBitmaskAtOffset bitmask cs 0
    !fwidth = sizeofArray fcs
    !twidth = sizeofArray tcs

