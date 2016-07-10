{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ents where

import ClassyPrelude.Yesod

import Database.Esqueleto.Join.TH

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
School
Teacher
  schoolId SchoolId
Student
  teacherId TeacherId
Pencil
  studentId StudentId
TwoKeys
  studentId1 StudentId
  studentId2 StudentId
|]

mkJoins
