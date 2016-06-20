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
  (JoinPair a b, Join (SqlExpr (Entity b) `InnerJoin` c)) =>
  Join (SqlExpr (Entity a) `InnerJoin` SqlExpr (Entity b) `InnerJoin` c) where
  join (a `InnerJoin` b `InnerJoin` c) = do
    join $ b `InnerJoin` c
    joinPair a b
instance
  (JoinPair a b, Join (SqlExpr (Entity b) `InnerJoin` c `InnerJoin` d)) =>
  Join (SqlExpr (Entity a) `InnerJoin` SqlExpr (Entity b) `InnerJoin` c `InnerJoin` d) where
  join (a `InnerJoin` b `InnerJoin` c `InnerJoin` d) = do
    join $ b `InnerJoin` c `InnerJoin` d
    joinPair a b
