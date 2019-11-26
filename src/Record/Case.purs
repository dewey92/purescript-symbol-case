module Record.Case where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Symbol.Case (class ToCamel)
import Prim.Row as Row
import Prim.RowList as RL
import Record (get)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

class ToCamelRecord (rl :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | rl -> from to where
  toCamelRecordImpl :: RLProxy rl -> { | row } -> Builder.Builder { | from } { | to }

instance rowToCamelNil :: ToCamelRecord RL.Nil row () () where
  toCamelRecordImpl _ _ = identity
instance rowToCamelCons
  ::
  ( IsSymbol label
  , IsSymbol camelLabel
  , ToCamel label camelLabel
  , Row.Cons label ty trash row
  , Row.Cons camelLabel ty to' to
  , Row.Lacks camelLabel to'
  , ToCamelRecord tail row from to'
  ) => ToCamelRecord (RL.Cons label ty tail) row from to where
  toCamelRecordImpl _ input = first <<< rest where
    labelP = SProxy :: SProxy camelLabel
    val = get (SProxy :: SProxy label) input
    first = Builder.insert labelP val
    rest = toCamelRecordImpl (RLProxy :: RLProxy tail) input

toCamelRecord :: ∀ rl input output.
  RL.RowToList input rl =>
  ToCamelRecord rl input () output =>
  Record input -> Record output
toCamelRecord input = Builder.build builder {}
  where
    builder = toCamelRecordImpl (RLProxy :: RLProxy rl) input
