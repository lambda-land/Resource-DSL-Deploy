module DSL.Driver where

import DSL.Model
import DSL.Profile
import DSL.Resource
import DSL.Serialize

runDriver = do
    dfus   <- readJSON "inbox" "dictionary"
    init   <- readJSON "inbox" "resources"
    model  <- readJSON "model" "model"
    -- config <- readJSON "inbox" "configuration"
    -- reqs   <- readJSON "inbox" "requirements"
    let dict = fmap Left dfus
    runWithDict dict init (loadModel model [])
