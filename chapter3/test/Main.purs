module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry )
import Data.Maybe (Maybe)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

{-
printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry book
-}

printEntry :: String -> String -> String -> AddressBook -> Maybe String
printEntry street city state book = showEntry <$> findEntry street city state book

main :: Eff (console :: CONSOLE) Unit
main = do
  let book1 = insertEntry example emptyBook

  {-
  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  -}

  logShow $ printEntry "123 Fake St." "Faketown" "CA" book0
  logShow $ printEntry "123 Fake St." "Faketown" "CA" book1
