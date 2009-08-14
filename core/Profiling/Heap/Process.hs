{-| This is a utility module to aid the construction of
'CreateProcess' structures with profiling parameters. -}

module Profiling.Heap.Process 
    ( ProfParam(..)
    , Breakdown(..)
    , Restriction(..)
    , processToProfile
    ) where

import System.Process
import Text.Printf

{-| The possible types of parameters. -}
data ProfParam
    -- | The type of breakdown.
    = PPBreakdown Breakdown
    -- | An additional filter on the runtime side.
    | PPRestriction Restriction [String]
    -- | Sampling interval in seconds.
    | PPInterval Float
    -- | Whether to include memory taken up by threads.
    | PPIncludeThreads
    -- | The maximum length of cost centre stack names.
    | PPNameLength Int
    -- | Retainer set size limit.
    | PPRetainerLimit Int

{-| The possible types of breakdowns. -}
data Breakdown
    -- | Breakdown by cost centre stack (origin of the data).
    = BCostCentreStack
    -- | Breakdown by module (code responsible for the data).
    | BModule
    -- | Breakdown by closure description (constructor name or some
    -- unique identifier).
    | BDescription
    -- | Breakdown by type (or an approximation if it is not known
    -- exactly).
    | BType
    -- | Breakdown by retainer set (effectively the entities that hold
    -- a direct reference to the data in question).
    | BRetainer
    -- | Breakdown by biography (phase of an object's lifetime).
    | BBiography

{-| The possible filters. Note that these are imposed by the runtime,
so we cannot override them on the application side. -}
data Restriction
    -- | Show only closures with one of the given names on the top of
    -- the cost centre stack.
    = RCCStackTop
    -- | Show only closures with one of the given names somewhere in
    -- the cost centre stack.
    | RCCStackAny
    -- | Show only closures produced by one of the given modules.
    | RModule
    -- | Show only closures with a description that matches one of the
    -- given names.
    | RDescription
    -- | Show only closures with one of the given types.
    | RType
    -- | Show only closures with retainer sets that contain at least
    -- one cost centre stack with a given name on the top.
    | RRetainer
    -- | Show only closures with one of the specified biographies,
    -- which must come from the set {lag, drag, void, use}.
    | RBiography

instance Show ProfParam where
    showsPrec _ (PPBreakdown b)      = showString "-h" . shows b
    showsPrec _ (PPRestriction r ns) = showString "-h" . shows r . showsNames ns
    showsPrec _ (PPInterval i)       = showString "-i" . showString (printf "%.2f" i)
    showsPrec _ (PPIncludeThreads)   = showString "-xt"
    showsPrec _ (PPNameLength l)     = showString "-L" . shows l
    showsPrec _ (PPRetainerLimit l)  = showString "-R" . shows l
    showList ps = foldr (\p s -> shows p . showChar ' ' . s) id ps

instance Show Breakdown where
    showsPrec _ BCostCentreStack = showChar 'c'
    showsPrec _ BModule          = showChar 'm'
    showsPrec _ BDescription     = showChar 'd'
    showsPrec _ BType            = showChar 'y'
    showsPrec _ BRetainer        = showChar 'r'
    showsPrec _ BBiography       = showChar 'b'

instance Show Restriction where
    showsPrec _ RCCStackTop  = showChar 'c'
    showsPrec _ RCCStackAny  = showChar 'C'
    showsPrec _ RModule      = showChar 'm'
    showsPrec _ RDescription = showChar 'd'
    showsPrec _ RType        = showChar 'y'
    showsPrec _ RRetainer    = showChar 'r'
    showsPrec _ RBiography   = showChar 'b'

showsNames :: [String] -> String -> String
showsNames []     = id
showsNames [n]    = showString n
showsNames (n:ns) = showString n . showChar ',' . showsNames ns

{-| A helper function to create a 'CreateProcess' structure. -}
processToProfile :: FilePath       -- ^ The executable to profile (relative paths start from the working directory).
                 -> Maybe FilePath -- ^ An optional working directory (inherited from the parent if not given).
                 -> [String]       -- ^ The list of parameters to pass to the program.
                 -> [ProfParam]    -- ^ Profiling parameters.
                 -> CreateProcess  -- ^ The resulting structure.
processToProfile exec dir params profParams = (proc exec allParams) { cwd = dir }
    where -- Note: this doesn't handle --RTS in the param string!
          allParams = params ++ (if null profParams then [] else "+RTS" : map show profParams)
