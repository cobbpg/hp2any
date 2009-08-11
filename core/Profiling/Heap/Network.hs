{-| This module provides functions to send and receive profiling
information over the network.  Currently the messages can only encode
'SinkInput' data. -}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Profiling.Heap.Network
    ( Message(..)
    , sendMsg
    , recvMsg
    , readMsg
    , writeMsg
    , putStream
    , getStream
    ) where

import Control.Applicative
import Control.Arrow
--import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Int
import Data.Maybe
import Data.List
import Profiling.Heap.Types
import System.IO

data Message = Stream SinkInput

sStrSample = "str_sample"
sStrName   = "str_name"
sStrStop   = "str_stop"

{-| Send a structured message over the network.  Can also be used for
logging into a file. -}

sendMsg :: Handle -> Message -> IO ()
sendMsg hdl = hPutStrLn hdl . writeMsg

{-| Receive a structured message over the network.  Can also be used
for parsing from a file. -}

recvMsg :: Handle -> IO (Maybe Message)
recvMsg hdl = readMsg <$> hGetLine hdl

{-| Parse a message. -}

readMsg :: String -> Maybe Message
readMsg = parseString messageParser

{-| Serialise a message. -}

writeMsg :: Message -> String
writeMsg = show

{-| Convert from callback data to message. -}

putStream :: SinkInput -> Message
putStream = Stream

{-| Extract callback data from message, if applicable. -}

getStream :: Message -> Maybe SinkInput
getStream (Stream dat) = Just dat
--getStream _            = Nothing

instance Show Message where
    showsPrec _ (Stream (SinkSample t ps)) = showString sStrSample . sepShows t . showPairs ps
    showsPrec _ (Stream (SinkId ccid name)) = showString sStrName . sepShows ccid . sepStr (S.unpack name)
    showsPrec _ (Stream SinkStop) = showString sStrStop

sep = showChar '\t'
sepStr s = sep . showString s
sepShows x = sep . shows x

showListMap g = foldr (\x f -> sep . g x . f) id
showPairs l = showListMap (\(x,y) -> shows x . sepShows y) l

-- * A minimal and rather dumb applicative parser, uulib style

-- pPref should really be a list of possible prefixes in order to be
-- able to implement the choice operator properly, but this simpler
-- version is perfectly fine for our purposes.
data MsgParser a = MP { pPref :: String, _pFun :: String -> Maybe (a,String) }

instance Functor MsgParser where
    fmap f (MP p g) = MP p ((fmap.fmap) (first f) g)

instance Applicative MsgParser where
    pure x = MP "" (Just . (,) x)
    MP pf gf <*> MP px gx = MP pf $ \s -> do
                                 (f,s') <- gf s
                                 s'' <- stripPrefix px s'
                                 (x,s''') <- gx s''
                                 return (f x,s''')

-- Shady business here: going from bottom to top!
instance Alternative MsgParser where
    empty = MP "" (const Nothing)
    MP p1 g1 <|> MP p2 g2 = MP "" $ \s ->
                            (stripPrefix p2 s >>= g2) <|> (stripPrefix p1 s >>= g1)

infixl 3 <||>

-- Alternative with cut (can fail on parseable strings if either p1 or
-- p2 is the prefix of the other, but it prevents a space leak if they
-- aren't). Yay for past obsession with Prolog.
(<||>) :: MsgParser a -> MsgParser a -> MsgParser a
MP p1 g1 <||> MP p2 g2 = MP "" $ \s ->
                         case stripPrefix p2 s of
                           Just s' -> g2 s'
                           Nothing -> case stripPrefix p1 s of
                                        Just s'' -> g1 s''
                                        Nothing -> Nothing

pInt :: MsgParser Int
pInt = MP "" $ listToMaybe . reads

pInt64 :: MsgParser Int64
pInt64 = MP "" $ listToMaybe . reads

pFrac :: MsgParser Double
pFrac = MP "" $ listToMaybe . reads

--pKey :: MsgParser String
--pKey = MP "" $ \str ->
--       let (pre,post) = span (`elem` '_':['a'..'z']) str
--       in if null pre then Nothing else Just (pre,post)

pChr :: Char -> MsgParser Char
pChr c = (pure c) { pPref = [c] }

pParam :: MsgParser String
pParam = MP "" $ Just . span (>=' ')

-- This can only be used at the end of a string, because:
-- * we assume one delimiter between items (and it can change...)
-- * the remainder is not preserved
pMany :: MsgParser a -> MsgParser [a]
pMany (MP p g) = MP "" $ \str ->
                 let rl s = case g =<< stripPrefix p s of
                              Nothing -> []
                              Just (v,s') -> v : if null s' then [] else rl (tail s')
                 in Just (rl str,"")

infixl 4 <=>
infixl 4 <->

(<=>) :: String -> a -> MsgParser a
s <=> v = (pure v) { pPref = s }

(<->) :: MsgParser (a -> b) -> MsgParser a -> MsgParser b
p1 <-> p2 = p1 <* pChr '\t' <*> p2

parseString :: MsgParser a -> String -> Maybe a
parseString (MP p g) s = fst <$> (g =<< stripPrefix p s)

messageParser :: MsgParser Message
messageParser =  sStrSample <=> (\t smp -> Stream (SinkSample t smp)) <-> pFrac <-> pProfSample
            <||> sStrName <=> (\ccid name -> Stream (SinkId ccid name)) <-> pInt <-> (S.pack <$> pParam)
            <||> sStrStop <=> Stream SinkStop
    where pProfSample = pMany ((,) <$> pInt <-> pInt64)
