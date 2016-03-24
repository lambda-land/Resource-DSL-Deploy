{-# LANGUAGE OverloadedStrings #-}

module Example.Stub where

import DSL.Expr
import DSL.Type


-- | Load the gzip library.
gzipT :: Schema Liquid
gzipT = Forall ["r"]
      $ (Just "m", tInt)
        :-> (Nothing, TRec [("Memory", Base (TInt, QEqu QThis (QRef "m")))] (Just "r"))
        :-> TRec [("Memory", Base (TInt, QEqu QThis (QAdd (QRef "m") (QI (-128))))),
                  ("GZip", tUnit)] (Just "r")

gzip :: Expr
gzip = Abs "rec"
     $ Ext "GZip" Unit
     $ update "Memory" (app2 (Ref "-") (Sel "Memory" (Ref "rec")) (I 128))
     $ Ref "rec"
