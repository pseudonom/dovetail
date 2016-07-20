{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Esqueleto.Join where

import Database.Esqueleto

type family Joins (a :: [*]) :: * where
  Joins (a ': rest) = JoinsInternal rest (SqlExpr (Entity a))
type family JoinsInternal (a :: [*]) (b :: *) :: * where
  JoinsInternal '[a] acc = InnerJoin acc (SqlExpr (Entity a))
  JoinsInternal (a ': rest) acc = JoinsInternal rest (InnerJoin acc (SqlExpr (Entity a)))

class FieldPair a b where
  type JoinKey a b
  pair :: (EntityField a (JoinKey a b), EntityField b (JoinKey a b))

class Split join where
  split :: a `join` b -> (a, b)
instance Split InnerJoin where
  split (a `InnerJoin` b) = (a, b)
instance Split LeftOuterJoin where
  split (a `LeftOuterJoin` b) = (a, b)
instance Split RightOuterJoin where
  split (a `RightOuterJoin` b) = (a, b)
instance Split FullOuterJoin where
  split (a `FullOuterJoin` b) = (a, b)
instance Split CrossJoin where
  split (a `CrossJoin` b) = (a, b)

split3 :: (Split join2, Split join1) => a `join1` b `join2` c -> (a `join1` b, b, c)
split3 abc =
  (ab, b, c)
    where
      (ab, c) = split abc
      (_, b) = split ab

class JoinPair a b where
  joinPair :: SqlExpr a -> SqlExpr b -> SqlQuery ()
instance (FieldPair a b, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Entity a) (Entity b) where
  joinPair a b =
    on (a ^. aField ==. b ^. bField)
      where
        (aField, bField) = pair
instance (FieldPair a b, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Maybe (Entity a)) (Maybe (Entity b)) where
  joinPair a b =
    on (a ?. aField ==. b ?. bField)
      where
        (aField, bField) = pair
instance (FieldPair a b, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Entity a) (Maybe (Entity b)) where
  joinPair a b =
    on (just (a ^. aField) ==. b ?. bField)
      where
        (aField, bField) = pair
instance (FieldPair a b, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Maybe (Entity a)) (Entity b) where
  joinPair a b =
    on (a ?. aField ==. just (b ^. bField))
     where
       (aField, bField) = pair

class Join a where
  join :: a -> SqlQuery ()
instance (JoinPair a b, Split join) => Join (SqlExpr a `join` SqlExpr b) where
  join ab = uncurry joinPair $ split ab
instance (Split join1, Split join2, JoinPair b c, Join (a `join1` SqlExpr b)) => Join (a `join1` SqlExpr b `join2` SqlExpr c) where
  join xs =
    joinPair l r *> join rest
    where
      (rest, l, r) = split3 xs
