module DSL.Path where

import Data.Data (Data,Typeable)
import GHC.Generics (Generic)

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow,throwM)

import Data.List.Split (splitOn)
import Data.String (IsString(..))

import DSL.Name


--
-- * Resource IDs
--

-- | Resource IDs are (absolute) paths from the root of the
--   resource environment.
newtype ResID = ResID [Name]
  deriving (Data,Eq,Generic,Monoid,Ord,Read,Show,Typeable)

-- | The root resource ID.
root :: ResID
root = ResID []

instance IsString ResID where
  fromString ('/':s) = ResID (splitOn "/" s)
  fromString s       = ResID (splitOn "/" s)


--
-- * Paths
--

-- | A path is either absolute or relative.
data PathKind = Absolute | Relative
  deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | A path through a resource environment. Unlike resource IDs, paths may be
--   relative, may contain "special" path names (such as ".." to refer to the
--   parent node), and may be appended with other paths. The idea is that a
--   resource ID is a simple key in the resource environment, while a path is
--   an intermediate object that can be manipulated in the language.
data Path = Path PathKind [Name]
  deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

-- | Errors that can occur when combining or converting paths.
data PathError
     = CannotAppend Path Path
     | CannotNormalize Path
  deriving (Data,Eq,Generic,Read,Show,Typeable)

instance Exception PathError

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

-- | Append two paths.
pathAppend :: MonadThrow m => Path -> Path -> m Path
pathAppend p1@(Path k1 l) p2@(Path k2 r) = case k2 of
    Relative -> pure (Path k1 (l ++ r))
    Absolute -> throwM (CannotAppend p1 p2)

-- | Convert a prefix + path into a resource ID. If the path is absolute, the
--   prefix is ignored. If the path is relative, it is appended to the prefix. 
--   The resulting path is normalized (i.e. special path names are eliminated)
--   to produce a resource ID.
toResID :: MonadThrow m => ResID -> Path -> m ResID
toResID (ResID pre) orig@(Path k p) = case k of
    Relative -> ResID <$> norm (pre ++ p)
    Absolute -> ResID <$> norm p
  where
    norm []         = pure []
    norm ("..":_)   = throwM (CannotNormalize orig)
    norm ("":p)     = norm p
    norm (".":p)    = norm p
    norm (_:"..":p) = norm p
    norm (n:p)      = (n:) <$> norm p

instance IsString Path where
  fromString ('/':s) = Path Absolute (splitOn "/" s)
  fromString s       = Path Relative (splitOn "/" s)
