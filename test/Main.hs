module Main where

import ClassyPrelude
import Database.Esqueleto

import Lib
import Ents

main :: IO ()
main = print ()

foo :: MonadIO m => ReaderT SqlBackend m [Entity Student]
foo =
  select . from $ \(ents@(_ `InnerJoin` _ `InnerJoin` student) :: Joins [School, Teacher, Student]) -> do
    joiner ents
    return student
