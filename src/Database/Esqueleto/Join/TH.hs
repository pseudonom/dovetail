{-# LANGUAGE TemplateHaskell #-}

module Database.Esqueleto.Join.TH where

import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Tuple
import qualified Database.Esqueleto as E
import Language.Haskell.TH

import Database.Esqueleto.Join

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = (f <$>) <$> a

type EntityType = Name
type FieldOutType = Name
type Constructor = Name

mkJoins :: Q [Dec]
mkJoins = mkInstances . findPairings =<< getEntityFields

getEntityFields :: Q [(EntityType, [(FieldOutType, Constructor)])]
getEntityFields = (\i -> (entityType i, catMaybes $ fieldKeyCons <$> entityFieldConstructors i)) <$$> entityFieldInstances

pairs :: [a] -> [(a, a)]
pairs xs = do
  a <- xs
  b <- xs
  pure (a, b)

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
        instance Join $(conT lType) $(conT rType) where
          join l r = E.on (l E.^. $(conE lCons) E.==. r E.^. $(conE rCons))
      |]

entityFieldInstances :: Q [Dec]
entityFieldInstances = do
  FamilyI _ instances <- reify $ mkName "EntityField"
  pure instances
entityType :: Dec -> Name
entityType (DataInstD _ _ [ConT ty, _] _ _) = ty
entityType _ = error "`EntityField` not returning `DataInstD`"
entityFieldConstructors :: Dec -> [Con]
entityFieldConstructors (DataInstD _ _ _ cons _) = cons
entityFieldConstructors _ = error "`EntityField` not returning `DataInstD`"

fieldKeyCons :: Con -> Maybe (Name, Name)
fieldKeyCons (ForallC [] [AppT _ (AppT (ConT k) (ConT ty))] (NormalC con _))
  | k == ''E.Key = Just (ty, con)
  | otherwise = Nothing
fieldKeyCons _ = Nothing
