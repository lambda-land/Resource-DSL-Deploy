
module DSL.Path where

import Control.Monad.Except

import Data.List.Split (splitOn)
import Data.String (IsString(..))

import DSL.Types


--
-- * Resource IDs
--


-- | The root resource ID.
root :: ResID
root = ResID []

instance IsString ResID where
  fromString ('/':s) = ResID (splitOn "/" s)
  fromString s       = ResID (splitOn "/" s)


-- | The root of an absolute path.
pathRoot :: Path
pathRoot = Path Absolute []

-- | A relative path indicating the current directory.
pathThis :: Path
pathThis = Path Relative ["."]

-- | A relative path indicating the parent.
pathParent :: Path
pathParent = Path Relative [".."]

-- | A relative path from a for-loop index.
pathFor :: Int -> Path
pathFor i = Path Relative [show i]

-- | Append two paths. If the second path is absolute, instead use it directly.
pathAppend :: Path -> Path -> Path
pathAppend _ p2@(Path Absolute _) = p2
pathAppend (Path k1 l) p2@(Path k2 r) = case k2 of
    Absolute -> p2
    Relative -> Path k1 (l ++ r)

-- | Convert a prefix + path into a resource ID. If the path is absolute, the
--   prefix is ignored. If the path is relative, it is appended to the prefix.
--   The resulting path is normalized (i.e. special path names are eliminated)
--   to produce a resource ID.
toResID :: MonadError VError m => ResID -> Path -> m ResID
toResID (ResID pre) orig@(Path k p) = case k of
    Relative -> ResID <$> norm (pre ++ p)
    Absolute -> ResID <$> norm p
  where
    norm []         = pure []
    norm ("..":_)   = throwError (One . PathE . CannotNormalize $ orig)
    norm ("":p)     = norm p
    norm (".":p)    = norm p
    norm (_:"..":p) = norm p
    norm (n:p)      = (n:) <$> norm p
