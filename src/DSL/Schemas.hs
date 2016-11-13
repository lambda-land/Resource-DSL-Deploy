{-# LANGUAGE OverloadedStrings #-}

module DSL.Schemas where

import qualified Data.Aeson             as A
import           Data.JSON.Schema

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
doStmtSchema = Object [Field {key="statement", required=True
                             , content=string}
                      ,Field {key="path", required=True, content=pathSchema}
                      ,Field {key="effect", required=True, content=effectSchema}
                      ]

ifStmtSchema :: Schema
ifStmtSchema = Object [Field {key="statement", required=True, content=string}
                      , Field {key="condition", required=True
                              , content=exprSchema}
                      , Field {key="then", required=True, content=blockSchema}
                      , Field {key="else", required=True, content=blockSchema}
                      ]

inStmtSchema :: Schema
inStmtSchema = Object [Field {key="statement", required=True, content=string}
                      , Field {key="context", required=True, content=pathSchema}
                      , Field {key="body", required=True, content=blockSchema}
                      ]

forStmtSchema :: Schema
forStmtSchema = Object [Field {key="statement", required=True, content=string}
                      , Field {key="variable", required=True, content=string}
                      , Field {key="maximum", required=True, content=exprSchema}
                      , Field {key="body", required=True, content=blockSchema}
                      ]

letStmtSchema :: Schema
letStmtSchema = Object [Field {key="statement", required=True, content=string}
                       , Field {key="variable", required=True, content=string}
                       , Field {key="bound", required=True, content=exprSchema}
                       , Field {key="body", required=True, content=blockSchema}
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

-- Profiles 
profileEffectSchema :: Schema
profileEffectSchema = Object [Field {key="key", required=True
                                    , content=pathSchema}
                             , Field {key = "value", required=True,
                                     content=Array
                                     unboundedLength
                                     False effectSchema}
                             ]

profileSchema :: Schema
profileSchema = Object [Field {key="parameters", required=True
                              , content=Array unboundedLength False paramSchema}
                       , Field {key="effects", required=True
                               , content=Array unboundedLength False
                                 profileEffectSchema}
                       ]

profileEntrySchema :: Schema
profileEntrySchema = Object [Field {key="type", required=True
                                   , content=string}
                            , Field {key="entry", required=True
                                    , content=profileSchema}]

-- Models
modelSchema :: Schema
modelSchema = Object [Field {key="parameters", required=True
                            , content=Array unboundedLength False paramSchema}
                     , Field {key="block", required=False, content=blockSchema}
                     ]

modelEntrySchema :: Schema
modelEntrySchema = Object [Field {key="type", required=True
                                   , content=string}
                          , Field {key="entry", required=True
                                   , content=modelSchema}
                          ]

-- Dictionaries
entrySchema :: Schema
entrySchema = Choice [profileEntrySchema, modelEntrySchema]

dictSchema :: Schema
dictSchema = Array unboundedLength False $
  Object [Field {key="key", required=True, content=string}
         , Field {key="value", required=True, content=entrySchema}
         ]

resourceSchema :: Schema
resourceSchema = Array unboundedLength False $
  Object [Field {key="key", required=True, content=resIDSchema}
         , Field {key="value", required=True, content=pTypeSchema}
         ]

