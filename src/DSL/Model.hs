module DSL.Model where

import Control.Monad (forM_)
import Data.Foldable (foldl')
import Data.SBV (bnot, (&&&))

import DSL.Types
import DSL.Effect
import DSL.Environment
import DSL.Expression
import DSL.Name
import DSL.Path
import DSL.Profile
import DSL.Resource
import DSL.SegList
import DSL.SAT


--
-- * Components
--



-- ** Operations

-- | Construct a model dictionary from an association list of models.
modelDict :: [(Name,Model)] -> Dictionary
modelDict l = envFromList [(Symbol n, ModEntry m) | (n,m) <- l]

-- | Construct a profile dictionary from an association list of models.
profileDict :: [(Name,Model)] -> Dictionary
profileDict l = envFromList [(Symbol n, ProEntry (toProfile m)) | (n,m) <- l]

vMergeEff :: Env Path (SegList Effect) -> (Path, Maybe BExpr, Effect) -> Env Path (SegList Effect)
vMergeEff env (p, d, e) | Just es <- envLookup' p env = envExtend p (segSetInsert d e es) env
                        | otherwise                   = envExtend p (segSetInsert d e []) env

vEnvFromList :: [(Path, Maybe BExpr, Effect)] -> Env Path (SegList Effect)
vEnvFromList = foldl' vMergeEff envEmpty

toProfileEntries :: Path -> Maybe BExpr -> Block -> [(Path, Maybe BExpr, Effect)]
toProfileEntries _ _ [] = []
toProfileEntries p d ((Elems xs):ys) = fromElems p d xs ++ toProfileEntries p d ys
  where
    fromElems _ _ [] = []
    fromElems p d ((In path blk):xs) = toProfileEntries (pathAppend p path) d blk ++ fromElems p d xs
    fromElems p d ((Do path eff):xs) = (pathAppend p path, d, eff):(fromElems p d xs)
    fromElems _ _ _ = error "toProfile: cannot convert model to profile"
toProfileEntries p Nothing ((Split d' l r):ys) = toProfileEntries p (Just d') l ++ toProfileEntries p (Just (bnot d')) r ++ toProfileEntries p Nothing ys
toProfileEntries p (Just d) ((Split d' l r):ys) = toProfileEntries p (Just (d &&& d')) l ++ toProfileEntries p (Just (d &&& (bnot d'))) r ++ toProfileEntries p (Just d) ys

toProfile :: Model -> Profile
toProfile (Model xs vstmts) =
    Profile xs (vEnvFromList (toProfileEntries pathThis Nothing vstmts))



-- TODO: convert profiles to models, compose profiles and models


-- ** Semantics

-- | Load a model into the current environment, prefixed by the given path.
loadModel :: MonadEval m => Model -> [V Expr] -> m ()
loadModel (Model xs block) args = withArgs xs args (execBlock block)

-- | Load a component by ID.
loadComp :: MonadEval m => CompID -> [V Expr] -> m ()
loadComp cid args = do
    dict <- getDict
    def <- promoteError (envLookup cid dict)
    case def of
      ProEntry profile -> loadProfile profile args
      ModEntry model   -> loadModel model args

-- | Execute a block of statements.
execBlock :: MonadEval m => Block -> m ()
execBlock = segMapM_ execStmt

-- | Execute a command in a sub-environment.
execInSub :: MonadEval m => Path -> m a -> m a
execInSub path mx = do
    rID <- getResID path
    withPrefix rID mx

-- | Execute a statement.
execStmt :: MonadEval m => Stmt -> m ()
-- apply an effect
execStmt (Do path eff) = do
    rID <- getResID path
    resolveEffect rID eff
-- conditional statement
execStmt stmt@(If cond tru fls) = unVM (do
    val <- evalExprV cond
    case val of
      B True  -> toVM $ execBlock tru
      B False -> toVM $ execBlock fls
      _ -> VM $ vError (StmtE $ StmtError stmt IfTypeError val)) >> return ()
-- do work in sub-environment
execStmt (In path body) = execInSub path (execBlock body)
-- loop over indexed sub-environments
execStmt stmt@(For var expr body) = unVM (do
    let iter i = execInSub (pathFor i) (withNewVar var (One. Just . I $ i) (execBlock body))
    val <- evalExprV expr
    case val of
      I n -> toVM $ forM_ [1..n] iter
      _ -> VM $ vError (StmtE $ StmtError stmt ForTypeError val)) >> return ()
-- extend the variable environment
execStmt (Let var expr body) = do
    val <- unVM $ evalExprV expr
    withNewVar var val (execBlock body)
-- load a sub-module or profile
execStmt stmt@(Load comp args) = unVM (do
    res <- evalExprV comp
    case res of
      S cid -> toVM $ loadComp cid args
      _ -> VM $ vError (StmtE $ StmtError stmt LoadTypeError res)) >> return ()

selectModel :: BExpr -> Model -> Model
selectModel d (Model ps blk) = Model (map (selectParam d) ps) (selectBlk d blk)

selectBlk :: BExpr -> Block -> Block
selectBlk _ [] = []
selectBlk d ((Elems xs):ys) = Elems (map (selectStmt d) xs) : selectBlk d ys
selectBlk d ((Split d' l r):xs) | d |=>| d' = selectBlk d l ++ selectBlk d xs
                                | d |=>!| d' = selectBlk d r ++ selectBlk d xs
                                | otherwise = Split d' (selectBlk d l) (selectBlk d r) : selectBlk d xs

selectStmt :: BExpr -> Stmt -> Stmt
selectStmt d (Do p e) = Do p (selectEff d e)
selectStmt d (If c t e) = If (selectExpr d c) (selectBlk d t) (selectBlk d e)
selectStmt d (In p blk) = In p (selectBlk d blk)
selectStmt d (For v e blk) = For v (selectExpr d e) (selectBlk d blk)
selectStmt d (Let v e blk) = Let v (selectExpr d e) (selectBlk d blk)
selectStmt d (Load e es) = Load (selectExpr d e) (map (selectExpr d) es)
