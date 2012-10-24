module Example where

import           Data.Set ( Set )
import           Model.Turing

import qualified Data.Set as S

data Q1 = Halt | Start | Read Binary deriving (Eq, Ord, Show)

tm1 :: TM Q1
tm1 = turingMachine Start $ S.fromList $ concat
    [ [ (Start,  b,  Read b,  O, R) | b <- [ O, I ] ]
    , [ (Read b, b', Read b', b, R) | b <- [ O, I ], b' <- [ O, I ] ] ]

instance Pretty Q1 where
    pretty Halt     = "H"
    pretty Start    = "S"
    pretty (Read I) = "i"
    pretty (Read O) = "o"

