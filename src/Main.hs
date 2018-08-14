{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import qualified Data.Set  as Set
import qualified Data.Text as Text

import Criterion.Measurement (initializeTime)
import Data.Set              (Set)
import Data.Text             (Text)
import System.IO             (BufferMode (NoBuffering), hSetBuffering, stdout)
import Control.DeepSeq       (NFData)
import GHC.Generics          (Generic)

import Criterion
import Criterion.Main


--------------------------------
-- === Running benchmarks === --
--------------------------------

iters :: Int
iters = 100000

src1 :: Text
src1 = Text.replicate iters "test"

data Grammar a
    = Tokens !(Set a) !(a -> Bool)
    | Many   !(Grammar a)
    | X      !(Grammar a)

instance Ord a => Semigroup (Grammar a) where
    Tokens s f <> Tokens s' g = Tokens (s <> s') $ \c -> f c || g c
    {-# INLINE (<>) #-}

token :: Eq a => a -> Grammar a
token = \a -> Tokens (Set.singleton a) (a ==)
{-# INLINE token #-}

many :: Grammar a -> Grammar a
many = Many
{-# INLINE many #-}

data Result
    = Success Text Text
    | Fail
    deriving (Show, Generic)

instance NFData Result


native :: Text -> Maybe Text
native src = out where
    tst = \c -> c == 't' || c == 'e' || c == 's' || c == 't'
    rs  = Text.takeWhile tst src
    out = Just rs
    
runTokenParser :: Grammar Char -> Text -> Result
runTokenParser = \grammar stream -> case grammar of
    Tokens _ tst -> let
        head = Text.head stream
        in if tst head
            then Success (Text.tail stream) (Text.singleton head)
            else Fail
    Many (Tokens _ tst) -> let
        (!consumed, !rest) = Text.span tst stream
        in Success rest consumed
    X !grammar -> runTokenParser grammar stream



test0 :: Text -> Result
test0 src = let
    s1 = token 't'
    s2 = token 'e'
    s3 = token 's'
    s4 = token 't'
    p  = many $! s1 <> s2 <> s3 <> s4
    in runTokenParser p src
{-# NOINLINE test0 #-}

testGrammar1 :: Grammar Char
testGrammar1 = let
    s1 = token 't'
    s2 = token 'e'
    s3 = token 's'
    s4 = token 't'
    in many $! s1 <> s2 <> s3 <> s4
{-# INLINE testGrammar1 #-}

test1 :: Text -> Result
test1 = runTokenParser testGrammar1
{-# NOINLINE test1 #-}

test2 :: Text -> Result
test2 src = let
    s1 = token 't'
    s2 = token 'e'
    s3 = token 's'
    s4 = token 't'
    p  = X $! many $! s1 <> s2 <> s3 <> s4
    in runTokenParser p src
{-# NOINLINE test2 #-}



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defaultMain
        [ env (pure src1) (\(~src) -> bench "test0" $ nf test0 src)
        , env (pure src1) (\(~src) -> bench "test1" $ nf test1 src)
        , env (pure src1) (\(~src) -> bench "test2" $ nf test2 src)
        , env (pure src1) (\(~src) -> bench "native" $ nf native src)
        ]