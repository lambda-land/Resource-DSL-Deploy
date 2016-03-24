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
