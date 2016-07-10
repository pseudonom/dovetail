{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Join where

import Database.Esqueleto

type family Joins (a :: [*]) :: * where
  Joins (a ': rest) = JoinsInternal rest (SqlExpr (Entity a))
type family JoinsInternal (a :: [*]) (b :: *) :: * where
  JoinsInternal '[a] acc = InnerJoin acc (SqlExpr (Entity a))
  JoinsInternal (a ': rest) acc = JoinsInternal rest (InnerJoin acc (SqlExpr (Entity a)))

class JoinPair a b where
  joinPair :: SqlExpr (Entity a) -> SqlExpr (Entity b) -> SqlQuery ()

class Join a where
  join :: a -> SqlQuery ()
instance JoinPair a b => Join (SqlExpr (Entity a) `InnerJoin` SqlExpr (Entity b)) where
  join (a `InnerJoin` b) = joinPair a b
instance
  (Join (a `InnerJoin` SqlExpr (Entity b)), JoinPair b c) =>
  Join (a `InnerJoin` SqlExpr (Entity b) `InnerJoin` SqlExpr (Entity c)) where
  join xs =
    joinPair l r *> join rest
    where
      (rest, l, r) = split xs

split :: a `InnerJoin` b `InnerJoin` c -> (a `InnerJoin` b, b, c)
split (a `InnerJoin` b `InnerJoin` c) = (a `InnerJoin` b, b, c)
