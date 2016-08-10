{-# LANGUAGE UndecidableInstances #-}

module Database.Esqueleto.Join where

import Data.Singletons.TH
import Database.Esqueleto
import Prelude

$(singletons [d|
  data MaybeCon = Present | Absent deriving (Eq, Show)
 |])

type family MaybeMaybe (a :: MaybeCon) (b :: *) :: * where
  MaybeMaybe 'Present b = Maybe b
  MaybeMaybe 'Absent b = b


type family Joins (a :: [*]) :: * where
  Joins (a ': rest) = JoinsInternal rest (SqlExpr (Entity a))
type family JoinsInternal (a :: [*]) (b :: *) :: * where
  JoinsInternal '[a] acc = InnerJoin acc (SqlExpr (Entity a))
  JoinsInternal (a ': rest) acc = JoinsInternal rest (InnerJoin acc (SqlExpr (Entity a)))

type PairSig a b c d =
  ( (SMaybeCon c, EntityField a (MaybeMaybe c (JoinKey a b)))
  , (SMaybeCon d, EntityField b (MaybeMaybe d (JoinKey a b)))
  )

class FieldPair a b c d | a b -> c, a b -> d where
  type JoinKey a b
  pair :: PairSig a b c d

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

instance (FieldPair a b c d, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Entity a) (Entity b) where
  joinPair a b =
    on condition
      where
        ((aMC, aField), (bMC, bField)) = pair :: PairSig a b c d
        condition =
          case (aMC, bMC) of
            (SAbsent, SAbsent) -> a ^. aField ==. b ^. bField
            (SPresent, SPresent) -> a ^. aField ==. b ^. bField
            (SPresent, SAbsent) -> a ^. aField ==. just (b ^. bField)
            (SAbsent, SPresent) -> just (a ^. aField) ==. b ^. bField

instance (FieldPair a b c d, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Maybe (Entity a)) (Maybe (Entity b)) where
  joinPair a b =
    on condition
      where
        ((aMC, aField), (bMC, bField)) = pair :: PairSig a b c d
        condition =
          case (aMC, bMC) of
            (SAbsent, SAbsent) -> a ?. aField ==. b ?. bField
            (SPresent, SPresent) -> a ?. aField ==. b ?. bField
            (SPresent, SAbsent) -> a ?. aField ==. just (b ?. bField)
            (SAbsent, SPresent) -> just (a ?. aField) ==. b ?. bField

instance (FieldPair a b c d, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Entity a) (Maybe (Entity b)) where
  joinPair a b =
    on condition
      where
        ((aMC, aField), (bMC, bField)) = pair :: PairSig a b c d
        condition =
          case (aMC, bMC) of
            (SAbsent, SAbsent) -> just (a ^. aField) ==. b ?. bField
            (SPresent, SPresent) -> just (a ^. aField) ==. b ?. bField
            (SPresent, SAbsent) -> a ^. aField ==. b ?. bField
            (SAbsent, SPresent) -> just (just (a ^. aField)) ==. b ?. bField

instance (FieldPair a b c d, PersistField (JoinKey a b), PersistEntity a, PersistEntity b) => JoinPair (Maybe (Entity a)) (Entity b) where
  joinPair a b =
    on condition
      where
        ((aMC, aField), (bMC, bField)) = pair :: PairSig a b c d
        condition =
          case (aMC, bMC) of
            (SAbsent, SAbsent) -> a ?. aField ==. just (b ^. bField)
            (SPresent, SPresent) -> a ?. aField ==. just (b ^. bField)
            (SPresent, SAbsent) -> a ?. aField ==. just (just (b ^. bField))
            (SAbsent, SPresent) -> a ?. aField ==. b ^. bField

class Join a where
  join :: a -> SqlQuery ()
instance (JoinPair a b, Split join) => Join (SqlExpr a `join` SqlExpr b) where
  join ab = uncurry joinPair $ split ab
instance (Split join1, Split join2, JoinPair b c, Join (a `join1` SqlExpr b)) => Join (a `join1` SqlExpr b `join2` SqlExpr c) where
  join xs =
    joinPair l r *> join rest
    where
      (rest, l, r) = split3 xs
