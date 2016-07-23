{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ents2 where

import ClassyPrelude.Yesod

import Ents1

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
Student
  teacherId (Key Teacher1)
|]
type StudentId1 = StudentId
