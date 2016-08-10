{-# LANGUAGE TemplateHaskell #-}

module Database.Esqueleto.Join.TH where

import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import qualified Database.Esqueleto as E
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

import Database.Esqueleto.Join

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = (f <$>) <$> a

type EntityType = Type
type FieldOutType = Type
type Constructor = Con

mkJoins :: Q [Dec]
mkJoins = mkInstances . findPairings =<< mapM pluck =<< entityFieldInstances

pluck :: Dec -> Q (EntityType, [(FieldOutType, Constructor, MaybeCon)])
pluck dec = (entityType dec, ) . catMaybes <$> mapM fieldKeyConstructors (entityFieldConstructors dec)

pairs :: [a] -> [(a, a)]
pairs xs = (,) <$> xs <*> xs

-- | Find pairs of entities with a unique way of joining
findPairings
  :: [(EntityType, [(FieldOutType, Constructor, MaybeCon)])]
  -> [((EntityType, Constructor, MaybeCon), (EntityType, Constructor, MaybeCon), FieldOutType)]
findPairings xs =
  symmetrize . catMaybes $ uncurry handlePair <$> pairs xs
    where
      symmetrize x = List.nub $ (swap <$> x) <> x -- Make sure we can join in either order
        where
          swap (l, r, f) = (r, l, f)
      handlePair (leftT, leftCons) (rightT, rightCons) =
        case (cons leftT leftCons, cons leftT rightCons) of
          ([(lCon, lMC)], [(rCon, rMC)])
            | not (lMC == Present && rMC == Present) -- It doesn't make much sense for the primary key to be nullable
            -> Just ((leftT, lCon, lMC), (rightT, rCon, rMC), AppT (ConT ''E.Key) leftT)
          _ -> Nothing
        where
          cons :: EntityType -> [(FieldOutType, Constructor, MaybeCon)] -> [(Constructor, MaybeCon)]
          cons t = map (\(_, c, mc) -> (c, mc)) . filter ((== t) . fst3)
          fst3 (a, _, _) = a

mkInstances :: [((EntityType, Constructor, MaybeCon), (EntityType, Constructor, MaybeCon), FieldOutType)] -> Q [Dec]
mkInstances = fmap concat . mapM (uncurry3 joinInstance)
  where
    uncurry3 f (a, b, c) = f a b c
    joinInstance (lType, lCons, lMC) (rType, rCons, rMC) fieldType =
      [d|
        instance FieldPair $(pure lType) $(pure rType) $(pure $ promote lMC) $(pure $ promote rMC) where
          type JoinKey $(pure lType) $(pure rType) = $(pure fieldType)
          pair =
            ( ($(singlize lMC), $(mkCon lCons))
            , ($(singlize rMC), $(mkCon rCons))
            )
      |]
        where
          promote Present = PromotedT 'Present
          promote Absent = PromotedT 'Absent
          singlize Present = [|SPresent|]
          singlize Absent = [|SAbsent|]
    mkCon (NormalC name _) = conE name
    mkCon _ = error "Field key doesn't use a normal constructor"

entityFieldInstances :: Q [Dec]
entityFieldInstances = do
  FamilyI _ instances <- reify ''E.EntityField
  pure instances

entityType :: Dec -> Type
entityType (DataInstD _ _ [ty, _] _ _) = ty
entityType _ = error "`EntityField` not returning `DataInstD`"

entityFieldConstructors :: Dec -> [Constructor]
entityFieldConstructors (DataInstD _ _ _ cons _) = cons
entityFieldConstructors _ = error "`EntityField` not returning `DataInstD`"

fieldKeyConstructors :: Constructor -> Q (Maybe (FieldOutType, Constructor, MaybeCon))
fieldKeyConstructors con =
  case con of
    (ForallC [] [AppT _equalityT ty] con') ->
      ((\(ty', mc) -> (ty', con', mc)) <$$>) . expandSyns' . extractEntityType =<< expandSyns ty
    _ -> pure Nothing
  where
    expandSyns' (Just (ty, con')) = Just . (, con') <$> expandSyns ty
    expandSyns' Nothing = pure Nothing
    extractEntityType (AppT (ConT k) ty)
      | k == ''E.Key = Just (ty, Absent)
    extractEntityType (AppT (ConT m) (AppT (ConT k) ty))
      | m == ''Maybe && k == ''E.Key = Just (ty, Present)
    extractEntityType _ = Nothing
