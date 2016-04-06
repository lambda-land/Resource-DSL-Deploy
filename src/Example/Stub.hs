module Example.Stub where

import DSL.Expr
import DSL.Type
import DSL.Predicate
import DSL.Primitive


-- | Load the gzip library.
gzip :: Expr Refined
gzip = Fun "r1" gzipT
     $ Ext "GZip" (Free Unit)
     $ Both (Sel "Memory" (Use "r1")) ("m","r2")
     $ Ext "Memory" (Use "m" - I 128) (Use "r2")

-- | Type of gzip expression.
gzipT :: Schema Refined
gzipT = Forall ["m","r"]
      $ polyRec "r" [("Memory", ("v",TInt) ?? IRef "v" .== IRef "m")]
    :-> polyRec "r" [("Memory", ("v",TInt) ?? IRef "v" .== IRef "m" - ILit 128),
                     ("GZip", Bang tUnit)]

-- | Initial resource environment.
initEnv :: Expr Refined
initEnv = rec [("Memory", I 200)]

-- | Required resulting resource type.
reqType :: Schema Refined
reqType = Forall ["r"]
        $ polyRec "r" [("Memory", ("v",TInt) ?? ILit 0 .<= IRef "v"),
                       ("GZip", Bang tUnit)]

-- | The final resource environment.
finalEnv :: Expr Refined
finalEnv = rec [("Memory", I 72), ("GZip", Free Unit)]

-- | The predicate to check. (This is an ad hoc construction.)
checkMe :: BPred
checkMe = (IRef "m1" .== ILit 200)
      &&& (IRef "m2" .== IRef "m1" - ILit 128)
      &&& (ILit 0 .<= IRef "m2")
