{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Profiling.Heap.Network
    ( Message(..)
    , StopTarget(..)
    , GraphMode(..)
    , OrderMode(..)
    , RgbValue(..)
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
import Data.Maybe
import Data.List
import Profiling.Heap.Types
import System.IO
import Text.Printf

--import System.IO.Unsafe

data Message
    -- | Costs at a given time.
    = StrSample Time ProfileSample
    -- | Cost centre id to name mapping.
    | StrName CostCentreId CostCentreName
    -- | End of input.
    | StrStop
    -- | Request interval between two given moments (one packet per sample).
    | DatReqInterval Time Time
    -- | One sample.
    | DatSample Time ProfileSample
    -- | End of samples.
    | DatEnd
    -- | Request id to name mapping.
    | DatReqNames
    -- | Name mapping.
    | DatNames [(CostCentreId,CostCentreName)]
    -- | Request length of profile.
    | DatReqTime
    -- | Time of last sample.
    | DatTime Time
    -- | Request to start profiling.  The executable field is mandatory.
    | CmdStart
      { mcExec :: Maybe FilePath -- Shouldn't be Nothing
      , mcCwd :: Maybe FilePath
      , mcParams :: Maybe String
      }
    -- | Request to stop the reader or the process profiled (or both).
    | CmdStop [StopTarget]
    -- | Configure viewport (negative position sets the right end).
    | CmdView
      { mcPos :: Maybe Time
      , mcZoomX :: Maybe Float
      , mcZoomY :: Maybe Float
      , mcMode :: Maybe GraphMode
      }
    -- | Set palette. Cost centres not mentioned should be lumped together as 'other'.
    | CmdSetPal [(CostCentreId,RgbValue)]
    -- | Only show cost centres with the ids in the list.
    | CmdFilter [CostCentreId]
    -- | Set ordering mode (unordered, integral, stddev or similar, explicitly given)
    | CmdOrder OrderMode
    -- | Request rendering (as geometrical data).
    | DispReqGraph
    -- | Data for one graph (geometry as rendered).
    | DispGraph CostCentreId RgbValue [(Time,Int,Int)]
    -- | End of graph data.
    | DispEnd
    -- | Error response with a unique error code and a textual explanation.
    | MsgError Int String
    -- | Acknowledge a successful request.
    | MsgOk String

sStrSample      = "str_sample"
sStrName        = "str_name"
sStrStop        = "str_stop"
sDatReqInterval = "dat_req_interval"
sDatSample      = "dat_sample"
sDatEnd         = "dat_end"
sDatReqNames    = "dat_req_names"
sDatNames       = "dat_names"
sDatReqTime     = "dat_req_time"
sDatTime        = "dat_time"
sCmdStart       = "cmd_start"
sCmdStop        = "cmd_stop"
sCmdView        = "cmd_view"
sCmdSetPal      = "cmd_setpal"
sCmdFilter      = "cmd_filter"
sCmdOrder       = "cmd_order"
sDispReqGraph   = "disp_req_graph"
sDispGraph      = "disp_graph"
sDispEnd        = "disp_end"
sMsgError       = "error"
sMsgOk          = "ok"

spExec  = "exec"
spCwd   = "cwd"
spPars  = "params"
spPos   = "pos"
spZoomX = "zoom_x"
spZoomY = "zoom_y"
spMode  = "mode"

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
putStream (SinkSample t smp) = StrSample t smp
putStream (SinkId ccid name) = StrName ccid name
putStream SinkStop = StrStop

{-| Extract callback data from message, if applicable. -}
getStream :: Message -> Maybe SinkInput
getStream (StrSample t smp)   = Just (SinkSample t smp)
getStream (StrName ccid name) = Just (SinkId ccid name)
getStream StrStop             = Just SinkStop
getStream _                   = Nothing

instance Show Message where
    showsPrec _ (StrSample t ps) = showString sStrSample . sepShows t . showPairs ps
    showsPrec _ (StrName ccid name) = showString sStrName . sepShows ccid . sepStr (S.unpack name)
    showsPrec _ (StrStop) = showString sStrStop
    showsPrec _ (DatReqInterval t1 t2) = showString sDatReqInterval . sepShows t1 . sepShows t2
    showsPrec _ (DatSample t ps) = showString sDatSample . sepShows t . showPairs ps
    showsPrec _ (DatEnd) = showString sDatEnd
    showsPrec _ (DatReqNames) = showString sDatReqNames
    showsPrec _ (DatNames names) = showString sDatNames . showNameMap names
    showsPrec _ (DatReqTime) = showString sDatReqTime
    showsPrec _ (DatTime t) = showString sDatTime . sepShows t
    showsPrec _ (CmdStart
                 { mcExec = exec
                 , mcCwd = cwd
                 , mcParams = params
                 }) = showString sCmdStart .
                      maybe id (showKey spExec . showString) exec .
                      maybe id (showKey spCwd . showString) cwd .
                      maybe id (showKey spPars . showString) params
    showsPrec _ (CmdStop ts) = showString sCmdStop . showParams ts
    showsPrec _ (CmdView
                 { mcPos = pos
                 , mcZoomX = zx
                 , mcZoomY = zy
                 , mcMode = mode
                 }) = showString sCmdView .
                      maybe id (showKey spPos . shows) pos .
                      maybe id (showKey spZoomX . shows) zx .
                      maybe id (showKey spZoomY . shows) zy .
                      maybe id (showKey spMode . shows) mode
    showsPrec _ (CmdSetPal pal) = showString sCmdSetPal . showPairs pal
    showsPrec _ (CmdFilter ids) = showString sCmdFilter . showParams ids
    showsPrec _ (CmdOrder om) = showString sCmdOrder . sepShows om
    showsPrec _ (DispReqGraph) = showString sDispReqGraph
    showsPrec _ (DispGraph ccid col vals) = showString sDispGraph . sepShows ccid .
                                            sepShows col . showTriples vals
    showsPrec _ (DispEnd) = showString sDispEnd
    showsPrec _ (MsgError code msg) = showString sMsgError . sepShows code . sepStr msg
    showsPrec _ (MsgOk cmd) = showString sMsgOk . sepStr cmd

sep = showChar '\t'
sepStr s = sep . showString s
sepShows x = sep . shows x
showKey s = ((sepStr s . showChar '=').)

showListMap g = foldr (\x f -> sep . g x . f) id
showPairs l = showListMap (\(x,y) -> shows x . sepShows y) l
showTriples l = showListMap (\(x,y,z) -> shows x . sepShows y . sepShows z) l
showNameMap = showListMap (\(ccid,name) -> shows ccid . sepStr (S.unpack name))
showParams l = showListMap shows l

data StopTarget = STReader | STProcess

sSTReader = "reader"
sSTProcess = "process"

instance Show StopTarget where
    showsPrec _ STReader = showString sSTReader
    showsPrec _ STProcess = showString sSTProcess

data GraphMode = GMAccumulated | GMSeparate

sGMAccumulated = "acc"
sGMSeparate = "sep"

instance Show GraphMode where
    showsPrec _ GMAccumulated = showString sGMAccumulated
    showsPrec _ GMSeparate = showString sGMSeparate

data OrderMode
    = OMNone
    | OMIntegral
    | OMVariation
    | OMCustom [CostCentreId]

sOMNone = "id"
sOMIntegral = "size"
sOMVariation = "variation"
sOMCustom = "custom"

instance Show OrderMode where
    showsPrec _ OMNone = showString sOMNone
    showsPrec _ OMIntegral = showString sOMIntegral
    showsPrec _ OMVariation = showString sOMVariation
    showsPrec _ (OMCustom ids) = showString sOMCustom . showParams ids

data RgbValue = RGB !Int !Int !Int

instance Show RgbValue where
    showsPrec _ (RGB r g b) = showString (printf "%02x%02x%02x" r g b)

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

pFrac :: MsgParser Double
pFrac = MP "" $ listToMaybe . reads

pKey :: MsgParser String
pKey = MP "" $ \str ->
       let (pre,post) = span (`elem` '_':['a'..'z']) str
       in if null pre then Nothing else Just (pre,post)

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
messageParser =  sStrSample <=> StrSample <-> pFrac <-> pProfSample
            <||> sStrName <=> StrName <-> pInt <-> (S.pack <$> pParam)
            <||> sStrStop <=> StrStop
            <||> sDatReqInterval <=> DatReqInterval <-> pFrac <-> pFrac
            <||> sDatSample <=> DatSample <-> pFrac <-> pProfSample
            <||> sDatEnd <=> DatEnd
            <||> sDatReqNames <=> DatReqNames
            <||> sDatNames <=> DatNames <-> pNameMap
            <||> sDatReqTime <=> DatReqTime
            <||> sDatTime <=> DatTime <-> pFrac
            <||> sCmdStart <=> makeCmdStart <-> pKeyVal
            <||> sCmdStop <=> CmdStop <-> pMany (sSTReader <=> STReader <||> sSTProcess <=> STProcess)
            <||> sCmdView <=> makeCmdView <-> pKeyVal
            <||> sCmdSetPal <=> CmdSetPal <-> pColourMap
            <||> sCmdFilter <=> CmdFilter <-> pMany pInt
            <||> sCmdOrder <=> CmdOrder <-> (sOMNone <=> OMNone
                                        <||> sOMIntegral <=> OMIntegral
                                        <||> sOMVariation <=> OMVariation
                                        <||> sOMCustom <=> OMCustom <-> pMany pInt)
            <||> sDispReqGraph <=> DispReqGraph
            <||> sDispGraph <=> DispGraph <-> pInt <-> (makeColour <$> pParam) <->
                                              pMany ((,,) <$> pFrac <-> pInt <-> pInt)

            <||> sDispEnd <=> DispEnd
            <||> sMsgError <=> MsgError <-> pInt <-> pParam
            <||> sMsgOk <=> MsgOk <-> pParam
    where pKeyVal = pMany ((,) <$> pKey <* pChr '=' <*> pParam)
          pProfSample = pMany ((,) <$> pInt <-> pInt)
          pNameMap = pMany ((,) <$> pInt <-> (S.pack <$> pParam))
          pColourMap = pMany ((,) <$> pInt <-> (makeColour <$> pParam))

          makeColour [rh,rl,gh,gl,bh,bl] = RGB (hex rh rl) (hex gh gl) (hex bh bl)
              where hex h l = read ['0','x',h,l]
          makeColour _ = RGB 0 0 0

          makeCmdStart = foldl' addParam (CmdStart Nothing Nothing Nothing)
              where addParam cmd (key,val)
                        | key == spExec = cmd { mcExec = Just val }
                        | key == spCwd  = cmd { mcCwd = Just val }
                        | key == spPars = cmd { mcParams = Just val }
                        | otherwise     = cmd

          makeCmdView = foldl' addParam (CmdView Nothing Nothing Nothing Nothing)
              where addParam cmd (key,val)
                        | key == spPos          = cmd { mcPos = Just (read val) }
                        | key == spZoomX        = cmd { mcZoomX = Just (read val) }
                        | key == spZoomY        = cmd { mcZoomY = Just (read val) }
                        | key == spMode &&
                          val == sGMSeparate    = cmd { mcMode = Just GMSeparate }
                        | key == spMode &&
                          val == sGMAccumulated = cmd { mcMode = Just GMAccumulated }
                        | otherwise             = cmd

_test :: IO ()
_test = mapM_ (print . maybe "---" writeMsg . readMsg)
       ["str_sample\t5.2\t3\t518\t12\t921\t6\t11442"
       ,"str_name\t4\thello/leo"
       ,"str_stop"
       ,"dat_req_interval\t3.1\t7"
       ,"dat_end"
       ,"dat_req_names"
       ,"dat_names\t4\thello/xy\t11\tI'm Here\t3\tanother_name(23)"
       ,"dat_req_time"
       ,"dat_time\t21"
       ,"cmd_start\tcwd=myDir\texec=myExec"
       ,"cmd_stop\treader\tprocess"
       ,"cmd_view\tmode=acc\tzoom_y=3.2\tpos=-13"
       ,"cmd_setpal\t4\t1050ff\t8\t883291"
       ,"cmd_filter\t12\t312\t9\t64"
       ,"cmd_order\tsize"
       ,"cmd_order\tcustom\t6\t9\t15"
       ,"disp_req_graph"
       ,"disp_graph\t7\t61ff20\t0.3\t10\t60\t0.9\t40\t90"
       ,"disp_end"
       ,"error\t1001\tYou're screwed."
       ,"ok\tcmd_start"
       ,"ok\t"
       ]

{-
General:

- delimiter is tab (\t), so spaces are allowed in arguments
- message ends with newline (\n)
- token notation:
  - <meaning> (straightforward type)
  - {type}
  - (alt1|alt2|...)
  - [optional]
  - zero_or_more*
  - one_or_more+
  - literal (any umarked string)
- argument order: those with <key>=<value> notation are interchangeable
- direction: -> to grapher, <- from grapher

Subtasks:

- stream profile:
  -> str_sample <t_smp> (<ccid> <cost>)+
     Costs at a given time.
  -> str_name <ccid> <name>
     Cost centre id to name mapping.
  -> str_stop
     End of input.
- profiling data (requests and responses):
  -> dat_req_interval <t_beg> <t_end>
     Request interval between two given moments (one packet per sample).
  <- dat_sample <t_smp> (<ccid> <cost>)+
     One sample.
  <- dat_end
     End of samples.
  -> dat_req_names
     Request id to name mapping.
  <- dat_names (<ccid> <name>)*
     Name mapping.
  -> dat_req_time
     Request length of profile.
  <- dat_time <t_max>
     Time of last sample.
- command:
  -> cmd_start exec={path} [cwd={path}] [params={string}]
     Request to start profiling.
  -> cmd_stop [reader] [process]
     Request to stop the reader or the process profiled (or both).
  -> cmd_view [pos=[-]{num}] [zoom_x={num}] [zoom_y={num}] [mode=(sep|acc)]
     Set viewport (- indicates that we're setting the position of the right end).
  -> cmd_setpal (<ccid> <col>)+
     Set palette. Cost centres not mentioned should be lumped together as 'other'.
  -> cmd_filter <ccid>+
     Only show cost centres with the ids in the list.
  -> cmd_order (id|size|variation|custom <ccid>+)
     Set ordering mode (unordered, integral, stddev or similar, explicitly given)
- display:
  -> disp_req_graph
     Request rendering.
  <- disp_graph <ccid> <col> (<t_smp> <val_bot> <val_top>)+
     Data for one graph (geometry as rendered).
  <- disp_end
     End of graph data.
- other:
  <- error <code> <string>
     Error response with a unique error code and a textual explanation.
  <- ok <command>
     Acknowledge a successful request.
-}
