module Example.Stub where

import Data.SBV (Boolean(..))

import DSL.Expr
import DSL.Type
import DSL.Predicate


-- | Load the gzip library.
gzip :: Expr Refined
gzip = Abs "rec" gzipT
     $ Ext "GZip" Unit
     $ update "Memory" (app2 (Ref "-") (Sel "Memory" (Ref "rec")) (I 128))
     $ Ref "rec"

-- | Type of gzip expression.
gzipT :: Schema Refined
gzipT = Forall ["m","r"]
      $ polyRec "r" [("Memory", ("x",TInt) ?? IRef "x" @== IRef "m")]
    :-> polyRec "r" [("Memory", ("x",TInt) ?? IRef "x" @== IRef "m" @- ILit (-128)),
                     ("GZip", tUnit)]

-- | Initial resource environment.
initEnv :: Expr Refined
initEnv = rec [("Memory", I 200)]

-- | Required resulting resource type.
reqType :: Schema Refined
reqType = Forall ["r"]
        $ polyRec "r" [("Memory", ("x",TInt) ?? ILit 0 @<= IRef "x"),
                       ("GZip", tUnit)]

-- | The final resource environment.
finalEnv :: Expr Refined
finalEnv = rec [("Memory", I 72), ("GZip", Unit)]

-- | The predicate to check. (This is an ad hoc construction.)
checkMe :: BPred
checkMe = (IRef "m1" @== ILit 200)
      &&& (IRef "m2" @== IRef "m1" @+ ILit (-128))
      &&& (ILit 0 @<= IRef "m2")
