module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr  = addr.street <> ", " <>
                    addr.city <> ", " <>
                    addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons 


{- different show entry function
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head 
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
-}

findEntry :: String -> String -> String -> AddressBook -> Maybe Entry
findEntry street city state = filter filterByAddress >>> head
  where
    filterByAddress :: Entry -> Boolean
    filterByAddress entry = entry.address.street == street && entry.address.city == city && entry.address.state == state

hasEntry :: Entry -> AddressBook -> Boolean
hasEntry entry = filter entryIsSame >>> not null
  where 
    entryIsSame :: Entry -> Boolean
    entryIsSame bookEntry = entry.firstName == bookEntry.firstName &&
                            entry.lastName == bookEntry.lastName &&
                            entry.address.street == bookEntry.address.street &&
                            entry.address.city == bookEntry.address.city &&
                            entry.address.state == bookEntry.address.state

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy (\a b = a.firstName == b.firstName && a.lastName == b.lastName)
