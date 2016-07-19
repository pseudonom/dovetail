{-# LANGUAGE TemplateHaskell #-}

module Database.Esqueleto.Join.TH where

import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Tuple
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

pluck :: Dec -> Q (EntityType, [(FieldOutType, Constructor)])
pluck dec = (entityType dec, ) . catMaybes <$> mapM fieldKeyConstructors (entityFieldConstructors dec)

pairs :: [a] -> [(a, a)]
pairs xs = (,) <$> xs <*> xs

-- | Find pairs of entities with a unique way of joining
findPairings
  :: [(EntityType, [(FieldOutType, Constructor)])]
  -> [((EntityType, Constructor), (EntityType, Constructor))]
findPairings xs =
  symmetrize . catMaybes $ uncurry handlePair <$> pairs xs
    where
      symmetrize x = List.nub $ (swap <$> x) <> x -- Make sure we can join in either order
      handlePair (leftT, leftCons) (rightT, rightCons) =
        case (cons leftT leftCons, cons leftT rightCons) of
          ([lCon], [rCon]) -> Just ((leftT, lCon), (rightT, rCon))
          _ -> Nothing
        where
          cons t = fmap snd . filter ((== t) . fst)

mkInstances :: [((EntityType, Constructor), (EntityType, Constructor))] -> Q [Dec]
mkInstances = fmap concat . mapM (uncurry joinInstance)
  where
    joinInstance (lType, lCons) (rType, rCons) =
      [d|
        instance JoinPair $(pure lType) $(pure rType) where
          joinPair l r = E.on (l E.^. $(mkCon lCons) E.==. r E.^. $(mkCon rCons))
      |]
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

fieldKeyConstructors :: Constructor -> Q (Maybe (Type, Constructor))
fieldKeyConstructors con = do
  case con of
    (ForallC [] [AppT _equalityT ty@(ConT _)] con) ->
      fmap (, con) . extractType <$> expandSyns ty
    (ForallC [] [AppT _equalityT ty] con) ->
      pure . fmap (, con) $ extractType ty
    _ -> pure Nothing
  where
    extractType (AppT (ConT k) ty)
      | k == ''E.Key = Just ty
    extractType _ = Nothing
