module Main where

import ClassyPrelude.Yesod hiding (Proxy, join)
import Control.Monad.Logger
import Database.Esqueleto
import Database.Persist.Sqlite

import Database.Esqueleto.Join
import Ents

main :: IO ()
main =
  runStderrLoggingT . withSqliteConn ":memory:" $ \backend ->
    flip runSqlConn backend $ do
      runMigration migrateAll
      void three
      void four


three :: MonadIO m => ReaderT SqlBackend m [Entity Student]
three =
  select . from $ \(ents@(_ `InnerJoin` _ `InnerJoin` student) :: Joins [School, Teacher, Student]) -> do
    join ents
    return student

type SEnt a = SqlExpr (Entity a)

four :: MonadIO m => ReaderT SqlBackend m [Entity Pencil]
four =
  select . from $ \ents@((_ :: SEnt School) `InnerJoin` (_ :: SEnt Teacher) `LeftOuterJoin` (_ :: SEnt Student) `LeftOuterJoin` pencil) -> do
    join ents
    return pencil
