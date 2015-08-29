module Test.Regex (tests) where

import Basics exposing (..)

import Regex exposing (..)

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)


tests : Test
tests =
  let simpleTests = suite "Simple Stuff"
        [ test "split All" <| assertEqual ["a", "b"] (split All (comma) "a,b")
        , test "split" <| assertEqual ["a","b,c"] (split (AtMost 1) (comma) "a,b,c")
        , test "find All" <| assertEqual
            ([Match "a" [] 0 1, Match "b" [] 1 2])
            (find All (case regex "." of Result v -> v) "ab")
        , test "find All" <| assertEqual
            ([Match "" [] 0 1])
            (find All (case regex ".*" of Result v -> v) "")

        , test "replace AtMost 0" <| assertEqual             "The quick brown fox"
            (replace (AtMost 0) (vowels) (\_ -> "") "The quick brown fox")

        , test "replace AtMost 1" <| assertEqual             "Th quick brown fox"
            (replace (AtMost 1) (vowels) (\_ -> "") "The quick brown fox")

        , test "replace AtMost 2" <| assertEqual             "Th qick brown fox"
            (replace (AtMost 2) (vowels) (\_ -> "") "The quick brown fox")

        , test "replace All" <| assertEqual           "Th qck brwn fx"
            (replace All (vowels) (\_ -> "") "The quick brown fox")
        ]
      
      vowels = case regex "[aeiou]" of Result x -> x
      comma = case regex "," of Result x -> x
  in
      suite "Regex" [ simpleTests ]
