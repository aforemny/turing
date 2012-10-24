{-# LANGUAGE FlexibleInstances #-}
module Model.Turing where

import           Control.Monad
import           Data.List
--import           Data.List.Split
import           Data.List.Zipper ( Zipper(..) )
import           Data.Map         ( Map )
import           Data.Maybe
import           Data.Set         ( Set )

import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Data.List.Zipper as Z

-- |Binary representatin

data Binary = I | O deriving (Eq, Ord, Show)

-- |Naturals

newtype Nat = Nat { unNat :: Int }

toNatUnsafe :: Int -> Nat
toNatUnsafe = Nat

toNat :: Int -> Nat
toNat n | n < 0     = error "toNat: negative argument"
        | otherwise = toNatUnsafe n

-- |Inidirectional infinite @Tape@

newtype Tape = Tape { unTape :: Zipper Binary }

fromInts :: [Int] -> Tape
fromInts = fromNats . map toNat

fromList :: [Binary] -> Tape
fromList = Tape . Zip [] . flip (++) (cycle [O])

fromNats :: [Nat] -> Tape
fromNats = fromList . intercalate [O] . map (flip replicate I . (+) 1 . unNat)

cursor :: Tape -> Binary
cursor = Z.cursor . unTape

replace :: Binary -> Tape -> Tape
replace b = Tape . Z.replace b . unTape

left, right :: Tape -> Tape
left  = Tape . Z.left  . unTape
right = Tape . Z.right . unTape

toListLeft, toListRight, toList :: Tape -> [Binary]
toListLeft  (Tape (Zip bs _)) = reverse bs
toListRight (Tape (Zip _ bs)) = bs
toList                        = Z.toList . unTape

-- |Turing machine

data TM q = TM
    { state :: q
    , opts  :: Map (q, Binary) (q, Binary, D)
    }

data D = L | R deriving (Eq, Ord, Show)

turingMachine :: Ord q => q -> Set (q, Binary, q, Binary, D) -> TM q
turingMachine q = TM q . M.fromList . map split . S.toList
  where split (q, b, q', b', d) = (,) (q, b) (q', b', d)

step :: Ord q => TM q -> Tape -> (TM q, Tape)
step tm tap = case M.lookup (state tm, cursor tap) (opts tm) of
   Just (q, b, d) -> (tm { state = q }, move d $ replace b $ tap)
   Nothing        -> (tm, tap)
  where
    move d = if d == L then left else right

-- |Processing

interactive :: (Ord q, Pretty q) => TM q -> Tape -> IO ()
interactive tm tap = forever $ do
    putStrLn $ curry pretty tm tap
    _ <- getLine
    uncurry interactive $ step tm tap
  where

    safeHead [] = Nothing
    safeHead xs = Just $ head xs

-- |Pretty printing

class Pretty a where
    pretty :: a -> String

instance Pretty q => Pretty (TM q, Tape) where
    pretty (tm, tap) = "OO" ++ concatMap show (take 64 $ toList tap) ++ "OO\n"
                            ++ "  " ++ replicate (length $ toListLeft tap) ' '
                            ++ "^" ++ "\n"
                            ++ "  " ++ replicate (length $ toListLeft tap) ' '
                            ++ pretty (state tm)

