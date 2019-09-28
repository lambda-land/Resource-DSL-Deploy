-- | Implementation of the external JSON interface to the challenge problem.
module DSL.Example.SwapDau.Serialize where

import Data.Aeson hiding (Value)
import Data.Aeson.BetterErrors
import Data.Aeson.Types (Pair, listValue)
import Data.Text (pack)
import qualified Data.Text as Text

import DSL.Environment
import DSL.Parser (parseExprText)
import DSL.Serialize

import DSL.Example.SwapDau.Types


--
-- * JSON serialization of BBN interface
--

instance ToJSON Rules where
  toJSON (MkRules m) = listValue rule (envToList m)
    where
      rule ((a,v),r) = object
        [ "Attribute" .= String a
        , "AttributeValue" .= toJSON v
        , case r of
            Compatible vs -> "Compatible" .= listValue toJSON vs
            Equation e    -> "Equation"   .= toJSON e
        ]

asRules :: ParseIt Rules
asRules = eachInArray rule >>= return . MkRules . envFromList
  where
    rule = do
      a <- key "Attribute" asText
      v <- key "AttributeValue" asPVal
      r <- Compatible <$> key "Compatible" (eachInArray asPVal)
             <|> Equation <$> key "Equation" asExpr'
      return ((a,v),r)
    -- TODO Alex's revised parser doesn't handle spaces right...
    -- this is a customized version of asExpr that works around this
    asExpr' = do
      t <- asText
      let t' = Text.filter (/= ' ') t
      case parseExprText t' of
        Right e  -> pure e
        Left msg -> throwCustomError (ExprParseError (pack msg) t)

instance ToJSON p => ToJSON (Dau p) where
  toJSON d = object
    [ "GloballyUniqueId"   .= String (dauID d)
    , "Port"               .= toJSON (ports d)
    , "BBNDauMonetaryCost" .= Number (fromInteger (toInteger (monCost d))) ]

asDau :: ParseIt a -> ParseIt (Dau (Ports a))
asDau asVal = do
    i <- key "GloballyUniqueId" asText
    ps <- key "Port" (eachInArray (asPort asVal))
    mc <- key "BBNDauMonetaryCost" asIntegral
    return (MkDau i ps mc)

portAttrsPairs :: ToJSON a => PortAttrs a -> [Pair]
portAttrsPairs = map entry . portAttrsToList
    where
      entry (k,v) = k .= toJSON v

asPortAttrs :: ParseIt a -> ParseIt (PortAttrs a)
asPortAttrs asVal = do
    kvs <- eachInObject asVal
    return (portAttrsFromList (filter isAttr kvs))
  where
    exclude = ["GloballyUniqueId", "BBNPortFunctionality"]
    isAttr (k,_) = not (elem k exclude)

instance ToJSON a => ToJSON (Port a) where
  toJSON p = object (pid : pfn : pattrs)
    where
      pid = "GloballyUniqueId" .= portID p
      pfn = "BBNPortFunctionality" .= portFunc p
      pattrs = portAttrsPairs (portAttrs p)

asPort :: ParseIt a -> ParseIt (Port a)
asPort asVal = do
    i <- key "GloballyUniqueId" asText
    fn <- key "BBNPortFunctionality" asText
    as <- asPortAttrs asVal
    return (MkPort i fn as)

instance ToJSON Constraint where
  toJSON (Exactly v)   = toJSON v
  toJSON (OneOf vs)    = listValue toJSON vs
  toJSON (Range lo hi) = object [ "Min" .= toJSON lo, "Max" .= toJSON hi ]
  toJSON (Sync ss)     = listValue (object . portAttrsPairs) ss

asConstraint :: ParseIt Constraint
asConstraint = Exactly <$> asPVal
    <|> OneOf <$> eachInArray asPVal
    <|> Range <$> key "Min" asInt <*> key "Max" asInt
    <|> Sync  <$> eachInArray (asPortAttrs asConstraint)
    <|> Sync . (:[]) <$> asPortAttrs asConstraint

instance ToJSON AttrVal where
  toJSON (Leaf v)  = toJSON v
  toJSON (Node as) = object (portAttrsPairs as)

asAttrVal :: ParseIt AttrVal
asAttrVal = Leaf <$> asPVal
    <|> Node <$> asPortAttrs asAttrVal

instance ToJSON SetInventory where
  toJSON s = object
    [ "daus" .= listValue toJSON (invDaus s) ]

asSetInventory :: ParseIt SetInventory
asSetInventory = MkSetInventory <$> key "daus" (eachInArray (asDau asConstraint))

instance ToJSON Request where
  toJSON r = object
    [ "daus" .= listValue toJSON (reqDaus r) ]

asRequest :: ParseIt Request
asRequest = MkRequest <$> key "daus" (eachInArray asRequestDau)

instance ToJSON RequestDau where
  toJSON r = case toJSON (reqDau r) of
      Object o -> Object (o <> attr)
      _ -> error "RequestDau#toJSON: internal error"
    where
      attr = "BBNDauFlaggedForReplacement" .= Bool (replace r)

asRequestDau :: ParseIt RequestDau
asRequestDau = do
    r <- key "BBNDauFlaggedForReplacement" asBool
    d <- asDau asConstraint
    return (MkRequestDau r d)

instance ToJSON Response where
  toJSON r = object
    [ "daus" .= listValue toJSON (resDaus r) ]

instance ToJSON ResponseDau where
  toJSON r = case toJSON (resDau r) of
      Object o -> Object (attr <> o)
      _ -> error "ResponseDau#toJSON: internal error"
    where
      attr = "SupersededGloballyUniqueIds" .= listValue String (oldDaus r)

instance ToJSON ResponsePort where
  toJSON p = case toJSON (resPort p) of
      Object o -> Object (attr <> o)
      _ -> error "ResponsePort#toJSON: internal error"
    where
      attr = "SupersededGloballyUniqueId" .= oldPort p

instance ToJSON Metrics where
  toJSON m = object
    [ "required-daus"             .= numReqDaus m
    , "required-ports"            .= numReqPorts m
    , "required-port-groups"      .= numReqGroups m
    , "daus-in-inventory"         .= numInvDaus m
    , "candidate-sub-inventories" .= numInvs m
    , "ignored-sub-inventories"   .= numIgnored m
    , "explored-sub-inventories"  .= numExplored m
    , "explored-ports"            .= numExpPorts m
    , "explored-port-groups"      .= numExpGroups m
    , "total-config-dimensions"   .= numCfgDims m
    , "total-match-dimensions"    .= numUseDims m
    ]
