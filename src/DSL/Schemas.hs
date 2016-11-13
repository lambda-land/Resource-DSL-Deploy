{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DSL.Schemas where

import DSL.Effect
import DSL.Environment
-- -- import DSL.Expression
import DSL.Model
import DSL.Parser
import DSL.Pretty
-- -- import DSL.Primitive
import DSL.Profile
import DSL.Resource

import DSL.Serialize
import qualified Data.Aeson             as A
import           Data.JSON.Schema
import           Data.JSON.Schema.Validate

-- Sugar
string = Value unboundedLength

-- Primitives
pTypeSchema :: Schema
pTypeSchema = Choice [Constant A.Null, Boolean, Number unbounded
                     , string]

pValSchema :: Schema
pValSchema = Choice [Constant A.Null, Boolean, Number unbounded
                    , string]

-- Paths
pathSchema :: Schema
pathSchema = string

resIDSchema :: Schema
resIDSchema = Array  unboundedLength False (string)

-- Expressions
paramSchema :: Schema
paramSchema = Object [Field {key="name", required=True, content=pTypeSchema}]

exprSchema :: Schema
exprSchema = string

funSchema :: Schema
funSchema = Object [Field {key="parameter", required=True, content=paramSchema}
                   , Field {key="body", required=True, content=exprSchema}
                   ]
-- Effects
createSchema :: Schema
createSchema = Object [Field {key="effect", required=True
                             , content=string}
                      , Field {key="expression", required=True
                              , content=exprSchema}]
  
checkSchema :: Schema
checkSchema = Object [Field {key="effect", required=True
                             , content=string}
                      , Field {key="function", required=True
                              , content=funSchema}]

modifySchema :: Schema
modifySchema =  checkSchema

deleteSchema :: Schema
deleteSchema = Object [Field {key="effect", required=True
                             , content=string}]
     
    

effectSchema :: Schema
effectSchema = Choice [createSchema, checkSchema, modifySchema, deleteSchema]

-- Statements
doStmtSchema :: Schema
doStmtSchema = undefined
