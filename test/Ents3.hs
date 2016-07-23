{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ents3 where

import ClassyPrelude.Yesod

import Database.Esqueleto.Join.TH
import Ents1 ()
import Ents2

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
Pencil
  studentId StudentId1
TwoKeys
  studentId1 StudentId
  studentId2 StudentId
|]


mkJoins

