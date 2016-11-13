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
doStmtSchema = Object [Field {key="statment", required=True
                             , content=string}
                      ,Field {key="path", required=True, content=pathSchema}
                      ,Field {key="effect", required=True, content=effectSchema}
                      ]

ifStmtSchema :: Schema
ifStmtSchema = Object [Field {key="statement", required=True, content=string}
                      , Field {key="condition", required=True
                              , content=exprSchema}
                      , Field {key="then", required=True, content=blockExpr}
                      , Field {key="else", required=True, content=blockExpr}
                      ]

inStmtSchema :: Schema
inStmtSchema = Object [Field {key="statement", required=True, content=string}
                      , Field {key="variable", required=True, content=string}
                      , Field {key="maximum", required=True, content=exprSchema}
                      , Field {key="body", required=True, content=blockExpr}
                      ]

letStmtSchema :: Schema
letStmtSchema = Object [Field {key="statement", required=True, content=string}
                       , Field {key="variable", required=True, content=string}
                       , Field {key="bound", required=True, content=exprSchema}
                       , Field {key="body", required=True, content=blockExpr}
                       ]

loadStmtSchema :: Schema
loadStmtSchema = Object [Field {key="statement", required=True, content=string}
                        , Field {key="component", required=True
                                , content=exprSchema}
                        , Field {key="arguments", required=True
                                , content=Array unboundedLength False exprSchema}
                        ]

stmtSchema :: Schema
stmtSchema = Choice [doStmtSchema, ifStmtSchema, inStmtSchema
                    , letStmtSchema, loadStmtSchema
                    ]

blockSchema :: Schema
blockSchema = Array unboundedLength False stmtSchema
