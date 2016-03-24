module Example.Stub where

import DSL.Type


-- | Load the gzip library.
gzip :: Schema Liquid
gzip = Forall ["r"]
     $ (Just "m", tBool)
       :-> (Nothing, Rec [("Memory", Base (TInt, QEqu QThis (QRef "m")))] (Just "r"))
       :-> Rec [("Memory", Base (TInt, QEqu QThis (QAdd (QRef "m") (QI (-128))))),
                ("GZip", tUnit)] (Just "r")
