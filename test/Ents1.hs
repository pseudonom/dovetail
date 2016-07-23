{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ents1 where

import ClassyPrelude.Yesod

share [mkPersist sqlSettings{ mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
School
Teacher
  schoolId (Key School)
|]
type Teacher1 = Teacher
