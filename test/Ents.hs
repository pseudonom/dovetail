{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ents where

import ClassyPrelude.Yesod

import TH

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
School
Teacher
  sol String
  schoolId SchoolId
Student
  teacherId TeacherId
|]

mkJoins
