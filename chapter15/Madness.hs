module Madness where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' exc adv noun adj =
  exc <> "! he said " <>
  adv <> "as he jumped into his car " <>
  noun <> "and drove off with his " <>
  adj <> "wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' exc adv noun adj =
  mconcat [
    exc, "! he said ",
    adv, "as he jumped into his car ",
    noun, "and drove off with his ",
    adj, "wife."
  ]
