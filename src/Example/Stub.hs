module Example.Stub where

import DSL.Expr
import DSL.Type


-- | Load the gzip library.
gzip :: Expr
gzip = Abs "rec"
     $ Ext "GZip" Unit
     $ update "Memory" (app2 (Ref "-") (Sel "Memory" (Ref "rec")) (I 128))
     $ Ref "rec"

-- | Type of gzip expression.
gzipT :: Schema Refined
gzipT = Forall ["m","r"]
      $ TRec [("Memory", Base (TInt, PEqu PThis (PRef "m")))] (Just "r")
    :-> TRec [("Memory", Base (TInt, PEqu PThis (PAdd (PRef "m") (PI (-128))))),
              ("GZip", tUnit)] (Just "r")

-- | Initial resource environment.
initEnv :: Expr
initEnv = Rec [("Memory", I 200)]

-- | Required resulting resource type.
reqType :: Schema Refined
reqType = Forall ["r"]
        $ TRec [("Memory", Base (TInt, PLte (PI 0) PThis)),
                ("GZip", tUnit)] (Just "r")

-- | The final resource environment.
finalEnv :: Expr
finalEnv = Rec [("Memory", I 72), ("GZip", Unit)]

-- | The predicate to check. (This is an ad hoc construction.)
checkMe :: Pred
checkMe = PEqu (PRef "m1") (PI 200)
   `PAnd` PEqu (PRef "m2") (PAdd (PRef "m1") (PI (-128)))
   `PAnd` PLte (PI 0) (PRef "m2")
