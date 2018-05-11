module DSL.Options where

import Options.Applicative

readRecord :: (Read a) => String -> ReadM a
readRecord name = eitherReader $ \arg -> case reads (name ++ " " ++ arg) of
  [(r, "")] -> return r
  _         -> Left $ "cannot parse value `" ++ arg ++ "'"
