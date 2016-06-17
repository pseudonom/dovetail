{-# LANGUAGE UndecidableInstances #-}
module Lib where

import Database.Esqueleto

type family Joins (a :: [*]) :: * where
  Joins (a ': rest) = JoinsInternal rest (SqlExpr (Entity a))
type family JoinsInternal (a :: [*]) (b :: *) :: * where
  JoinsInternal '[a] acc = InnerJoin acc (SqlExpr (Entity a))
  JoinsInternal (a ': rest) acc = JoinsInternal rest (InnerJoin acc (SqlExpr (Entity a)))

class Join a b where
  join :: SqlExpr (Entity a) -> SqlExpr (Entity b) -> SqlQuery ()

class Joiner a where
  joiner :: a -> SqlQuery ()
instance Join a b => Joiner (SqlExpr (Entity a) `InnerJoin` SqlExpr (Entity b)) where
  joiner (a `InnerJoin` b) = join a b
instance
  (Join a b, Joiner (SqlExpr (Entity b) `InnerJoin` c)) =>
  Joiner (SqlExpr (Entity a) `InnerJoin` SqlExpr (Entity b) `InnerJoin` c) where
  joiner (a `InnerJoin` b `InnerJoin` c) = do
    joiner $ b `InnerJoin` c
    join a b
