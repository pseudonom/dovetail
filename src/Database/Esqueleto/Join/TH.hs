{-# LANGUAGE TemplateHaskell #-}

module Database.Esqueleto.Join.TH where

import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Tagged
import qualified Database.Esqueleto as E
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

import Database.Esqueleto.Join

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = (f <$>) <$> a

mkJoins :: Q [Dec]
mkJoins = fmap concat . mapM mkInstance . findPairings =<< mapM pluck =<< entityFieldInstances

data Entity
  = Entity
  { eEntityType :: Tagged "Entity" Type -- ^ Type like @Student@
  , eFields :: [EntityField] -- ^ Foreign key fields like @StudentTeacherId@
  } deriving (Eq, Show)

pluck :: Tagged "EntityField" Dec -> Q Entity
pluck dec = Entity (entityType dec) . catMaybes <$> mapM fieldKeyConstructors (entityFieldConstructors dec)

pairs :: [a] -> [(a, a)]
pairs xs = (,) <$> xs <*> xs

data InstanceEntity -- | One piece of @FieldPair@ declaration like a @Student@ in @Student@-@Teacher@ pair
  = InstanceEntity
  { ieEntityType :: Tagged "Entity" Type
  , ieFieldConstructor :: Tagged "FieldConstructor" Con
  , ieMaybeCon :: MaybeCon
  } deriving (Eq, Show)

data Pair
  = Pair
  { left :: InstanceEntity
  , right :: InstanceEntity
  , joinType :: Tagged "JoinType" Type -- ^ The type that these two entities can join on like @TeacherId@ in a @Student@-@Teacher@ pair
  } deriving (Eq, Show)

-- | Find pairs of entities with a unique way of joining
findPairings :: [Entity] -> [Pair]
findPairings xs =
  symmetrize . catMaybes $ uncurry handlePair <$> pairs xs
    where
      symmetrize x = List.nub $ (swap <$> x) <> x -- Make sure we can join in either order
        where
          swap Pair{..} = Pair right left joinType
      handlePair lEnt rEnt =
        case (cons (eEntityType lEnt) (eFields lEnt), cons (eEntityType lEnt) (eFields rEnt)) of
          ([(lFC, lMC)], [(rFC, rMC)])
            | not (lMC == Present && rMC == Present) -- It doesn't make much sense for the primary key to be nullable
            -> Just (Pair (InstanceEntity (eEntityType lEnt) lFC lMC) (InstanceEntity (eEntityType rEnt) rFC rMC) (Tagged . AppT (ConT ''E.Key) . unTagged . eEntityType $ lEnt))
          _ -> Nothing
        where
          cons :: Tagged "Entity" Type  -> [EntityField] -> [(Tagged "FieldConstructor" Con, MaybeCon)]
          cons t =
            map (\EntityField{..} -> (efFieldConstructor, efMaybeCon)) .
            filter ((== unTagged t) . unTagged . efFieldOutType)

mkInstance :: Pair -> Q [Dec]
mkInstance Pair{..} =
  [d|
    instance FieldPair $(spliceTCon left) $(spliceTCon right) $(spliceMaybeCon left) $(spliceMaybeCon right) where
      type JoinKey $(spliceTCon left) $(spliceTCon right) = $(pure . unTagged $ joinType)
      pair =
        ( ($(singlize . ieMaybeCon $ left), $(spliceCon left))
        , ($(singlize . ieMaybeCon $ right), $(spliceCon right))
        )
  |]
    where
      promote Present = PromotedT 'Present
      promote Absent = PromotedT 'Absent
      singlize Present = [|SPresent|]
      singlize Absent = [|SAbsent|]
      spliceMaybeCon = pure . promote . ieMaybeCon
      spliceTCon = pure . unTagged . ieEntityType
      spliceCon = mkCon . unTagged . ieFieldConstructor
      mkCon (NormalC name _) = conE name
      mkCon _ = error "Field key doesn't use a normal constructor"

entityFieldInstances :: Q [Tagged "EntityField" Dec]
entityFieldInstances = do
  FamilyI _ instances <- reify ''E.EntityField
  pure $ Tagged <$> instances

entityType :: Tagged "EntityField" Dec -> Tagged "Entity" Type
entityType (Tagged (DataInstD _ _ [ty, _] _ _)) = Tagged ty
entityType _ = error "`EntityField` not returning `DataInstD`"

entityFieldConstructors :: Tagged "EntityField" Dec -> [Tagged "ForAllFieldConstructor" Con]
entityFieldConstructors (Tagged (DataInstD _ _ _ cons _)) = Tagged <$> cons
entityFieldConstructors _ = error "`EntityField` not returning `DataInstD`"

data EntityField
  = EntityField
  { efFieldOutType :: Tagged "FieldOutType" Type -- ^ In a field like @StudentTeacherId@, the @FieldOutType@ is @Teacher@
  , efFieldConstructor :: Tagged "FieldConstructor" Con -- ^ A constructor like @StudentTeacherId@
  , efMaybeCon :: MaybeCon -- ^ Does the @FieldConstructor@ return a type like @Maybe TeacherId@ or just @TeacherId@?
  } deriving (Eq, Show)

fieldKeyConstructors :: Tagged "ForAllFieldConstructor" Con -> Q (Maybe EntityField)
fieldKeyConstructors (Tagged con) =
  case con of
    (ForallC [] [AppT _equalityT ty] con') ->
      (uncurry (mkEntityField con') <$$>) . expandSyns' . extractEntityType =<< expandSyns ty
    _ -> pure Nothing
  where
    mkEntityField (Tagged -> efFieldConstructor) (Tagged -> efFieldOutType) efMaybeCon = EntityField{..}
    expandSyns' (Just (ty, con')) = Just . (, con') <$> expandSyns ty
    expandSyns' Nothing = pure Nothing
    extractEntityType (AppT (ConT k) ty)
      | k == ''E.Key = Just (ty, Absent)
    extractEntityType (AppT (ConT m) (AppT (ConT k) ty))
      | m == ''Maybe && k == ''E.Key = Just (ty, Present)
    extractEntityType _ = Nothing
