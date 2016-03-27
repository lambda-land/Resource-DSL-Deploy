module Example.Stub where

import Data.SBV (Boolean(..))

import DSL.Expr
import DSL.Type
import DSL.Predicate


-- | Load the gzip library.
gzip :: Expr
gzip = Abs "rec"
     $ Ext "GZip" Unit
     $ update "Memory" (app2 (Ref "-") (Sel "Memory" (Ref "rec")) (I 128))
     $ Ref "rec"

-- | Type of gzip expression.
gzipT :: Schema Refined
gzipT = Forall ["m","r"]
      $ TRec [("Memory", ("x",TInt) ?? IRef "x" @== IRef "m")] (Just "r")
    :-> TRec [("Memory", ("x",TInt) ?? IRef "x" @== IRef "m" @- ILit (-128)),
              ("GZip", tUnit)] (Just "r")

-- | Initial resource environment.
initEnv :: Expr
initEnv = Rec [("Memory", I 200)]

-- | Required resulting resource type.
reqType :: Schema Refined
reqType = Forall ["r"]
        $ TRec [("Memory", ("x",TInt) ?? ILit 0 @<= IRef "x"),
                ("GZip", tUnit)] (Just "r")

-- | The final resource environment.
finalEnv :: Expr
finalEnv = Rec [("Memory", I 72), ("GZip", Unit)]

-- | The predicate to check. (This is an ad hoc construction.)
checkMe :: BPred
checkMe = (IRef "m1" @== ILit 200)
      &&& (IRef "m2" @== IRef "m1" @+ ILit (-128))
      &&& (ILit 0 @<= IRef "m2")
