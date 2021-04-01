-- | Various string conversions for analysis modules, and a 

{-# LANGUAGE TupleSections #-}

module Language.Fortran.Analysis.Pretty where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.BBlocks (SuperBBGr, superBBGrGraph, superBBGrClusters)

import           Data.Data
import           Data.Generics.Uniplate.Data (universeBi)
import           Data.Maybe (isJust)
import           Text.PrettyPrint.GenericPretty (pretty, Out)
import           Data.List (intercalate, unfoldr)
import           Control.Monad.Writer
import qualified Data.IntMap as IM
import           Data.Graph.Inductive

-- -----------------------------------------------------------------------------
-- * Basic pretty printing for analysis types

-- | Show a basic block graph in a somewhat decent way.
showBBGr :: (Out a, Show a) => BBGr a -> String
showBBGr (BBGr gr _ _) = execWriter . forM (labNodes gr) $ \ (n, bs) -> do
  let b = "BBLOCK " ++ show n ++ " -> " ++ show (map (\ (_, m, _) -> m) $ out gr n)
  tell $ "\n\n" ++ b
  tell $ "\n" ++ replicate (length b) '-' ++ "\n"
  tell (((++"\n") . pretty) =<< bs)

-- | Show a basic block graph without the clutter
showAnalysedBBGr :: (Out a, Show a) => BBGr (Analysis a) -> String
showAnalysedBBGr = showBBGr . bbgrMap (nmap strip)
  where
    strip = map (fmap insLabel)

-- | Show a basic block supergraph
showSuperBBGr :: (Out a, Show a) => SuperBBGr (Analysis a) -> String
showSuperBBGr = showAnalysedBBGr . superBBGrGraph

-- | Pick out and show the basic block graphs in the program file analysis.
showBBlocks :: (Data a, Out a, Show a) => ProgramFile (Analysis a) -> String
showBBlocks pf = perPU =<< getPUs pf
  where
    perPU PUComment{} = ""
    perPU pu | Analysis { bBlocks = Just gr } <- getAnnotation pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ showBBGr (bbgrMap (nmap strip) gr) ++ "\n\n"
      where p = "| Program Unit " ++ show (puName pu) ++ " |"
            dashes = replicate (length p) '-'
    perPU pu =
      dashes ++ "\n" ++ p ++ "\n" ++ dashes ++ "\n" ++ unlines (map (pretty . fmap insLabel) (programUnitBody pu)) ++ "\n\n"
      where p = "| Program Unit " ++ show (puName pu) ++ " |"
            dashes = replicate (length p) '-'
    strip = map (fmap insLabel)
    getPUs :: Data a => ProgramFile (Analysis a) -> [ProgramUnit (Analysis a)]
    getPUs = universeBi

-- -----------------------------------------------------------------------------
-- * Alternate AST pretty printer

-- Neither Show nor 'Text.PrettyPrint'. Used by the DOT converters.

showPUName :: ProgramUnitName -> String
showPUName (Named n) = n
showPUName NamelessBlockData = ".blockdata."
showPUName NamelessMain = ".main."
showPUName NamelessComment = ".comment."

showBlock :: Block a -> String
showBlock (BlStatement _ _ mlab st)
    | null (str :: String) = ""
    | otherwise = showLab mlab ++ str ++ "\\l"
  where
    str =
      case st of
        StExpressionAssign _ _ e1 e2 -> showExpr e1 ++ " <- " ++ showExpr e2
        StIfLogical _ _ e1 _         -> "if " ++ showExpr e1
        StWrite _ _ _ (Just aexps)   -> "write " ++ aIntercalate ", " showExpr aexps
        StPrint _ _ _ (Just aexps)   -> "print " ++ aIntercalate ", " showExpr aexps
        StCall _ _ cn _              -> "call " ++ showExpr cn
        StDeclaration _ _ ty Nothing adecls ->
          showType ty ++ " " ++ aIntercalate ", " showDecl adecls
        StDeclaration _ _ ty (Just aattrs) adecls ->
          showType ty ++ " " ++
            aIntercalate ", " showAttr aattrs ++
            aIntercalate ", " showDecl adecls
        StDimension _ _ adecls       -> "dimension " ++ aIntercalate ", " showDecl adecls
        StExit{}                     -> "exit"
        _                            -> "<unhandled statement: " ++ show (toConstr (fmap (const ()) st)) ++ ">"
showBlock (BlIf _ _ mlab _ (Just e1:_) _ _) = showLab mlab ++ "if " ++ showExpr e1 ++ "\\l"
showBlock (BlDo _ _ mlab _ _ (Just spec) _ _) =
    showLab mlab ++ "do " ++ showExpr e1 ++ " <- " ++
      showExpr e2 ++ ", " ++
      showExpr e3 ++ ", " ++
      maybe "1" showExpr me4 ++ "\\l"
  where DoSpecification _ _ (StExpressionAssign _ _ e1 e2) e3 me4 = spec
showBlock (BlDo _ _ _ _ _ Nothing _ _) = "do"
showBlock (BlComment{})                = ""
showBlock b = "<unhandled block: " ++ show (toConstr (fmap (const ()) b)) ++ ">"

showAttr :: Attribute a -> String
showAttr (AttrParameter _ _) = "parameter"
showAttr (AttrPublic _ _) = "public"
showAttr (AttrPrivate _ _) = "private"
showAttr (AttrProtected _ _) = "protected"
showAttr (AttrAllocatable _ _) = "allocatable"
showAttr (AttrAsynchronous _ _) = "asynchronous"
showAttr (AttrDimension _ _ aDimDecs) =
  "dimension ( " ++ aIntercalate ", " showDim aDimDecs ++ " )"
showAttr (AttrExternal _ _) = "external"
showAttr (AttrIntent _ _ In) = "intent (in)"
showAttr (AttrIntent _ _ Out) = "intent (out)"
showAttr (AttrIntent _ _ InOut) = "intent (inout)"
showAttr (AttrIntrinsic _ _) = "intrinsic"
showAttr (AttrOptional _ _) = "optional"
showAttr (AttrPointer _ _) = "pointer"
showAttr (AttrSave _ _) = "save"
showAttr (AttrTarget _ _) = "target"
showAttr (AttrValue _ _) = "value"
showAttr (AttrVolatile _ _) = "volatile"
showAttr (AttrSuffix _ _ (SfxBind _ _ Nothing)) = "bind(c)"
showAttr (AttrSuffix _ _ (SfxBind _ _ (Just e))) = "bind(c,name=" ++ showExpr e ++ ")"

showLab :: Maybe (Expression a) -> String
showLab a =
  case a of
    Nothing -> replicate 6 ' '
    Just (ExpValue _ _ (ValInteger l)) -> ' ':l ++ replicate (5 - length l) ' '
    _ -> error "unhandled showLab"

showValue :: Value a -> Name
showValue (ValVariable v)       = v
showValue (ValIntrinsic v)      = v
showValue (ValInteger v)        = v
showValue (ValReal v)           = v
showValue (ValComplex e1 e2)    = "( " ++ showExpr e1 ++ " , " ++ showExpr e2 ++ " )"
showValue (ValString s)         = "\\\"" ++ escapeStr s ++ "\\\""
showValue v                     = "<unhandled value: " ++ show (toConstr (fmap (const ()) v)) ++ ">"

escapeStr :: String -> String
escapeStr = map fst . unfoldr f . map (,False)
  where
    f []                = Nothing
    f ((c,False):cs)
      | c `elem` "\"\\" = Just (('\\', False), (c, True):cs)
    f ((c,_):cs)        = Just ((c, False), cs)

showExpr :: Expression a -> String
showExpr (ExpValue _ _ v)         = showValue v
showExpr (ExpBinary _ _ op e1 e2) = "(" ++ showExpr e1 ++ showOp op ++ showExpr e2 ++ ")"
showExpr (ExpUnary _ _ op e)      = "(" ++ showUOp op ++ showExpr e ++ ")"
showExpr (ExpSubscript _ _ e1 aexps) = showExpr e1 ++ "[" ++
                                       aIntercalate ", " showIndex aexps ++ "]"
showExpr e                        = "<unhandled expr: " ++ show (toConstr (fmap (const ()) e)) ++ ">"

showIndex :: Index a -> String
showIndex (IxSingle _ _ _ i) = showExpr i
showIndex (IxRange _ _ l u s) =
  maybe "" showExpr l ++ -- Lower
  ':' : maybe "" showExpr u ++ -- Upper
  maybe "" (\u' -> ':' : showExpr u') s -- Stride

showUOp :: UnaryOp -> String
showUOp Plus = "+"
showUOp Minus = "-"
showUOp Not = "!"
-- needs a custom instance
showUOp (UnCustom x) = show x

showOp :: BinaryOp -> String
showOp Addition = " + "
showOp Multiplication = " * "
showOp Subtraction = " - "
showOp Division = " / "
showOp Concatenation = " // "
showOp op = " ." ++ show op ++ ". "

showType :: TypeSpec a -> String
showType (TypeSpec _ _ t (Just _)) = showBaseType t ++ "(selector)" -- ++ show s
showType (TypeSpec _ _ t Nothing)  = showBaseType t

showBaseType :: BaseType -> String
showBaseType TypeInteger         = "integer"
showBaseType TypeReal            = "real"
showBaseType TypeDoublePrecision = "double"
showBaseType TypeComplex         = "complex"
showBaseType TypeDoubleComplex   = "doublecomplex"
showBaseType TypeLogical         = "logical"
showBaseType (TypeCharacter l k) = case (l, k) of
  (Just cl, Just ki) -> "character(" ++ showCharLen cl ++ "," ++ ki ++ ")"
  (Just cl, Nothing) -> "character(" ++ showCharLen cl ++ ")"
  (Nothing, Just ki) -> "character(kind=" ++ ki ++ ")"
  (Nothing, Nothing) -> "character"
showBaseType (TypeCustom s)      = "type(" ++ s ++ ")"
showBaseType TypeByte            = "byte"
showBaseType ClassStar           = "class(*)"
showBaseType (ClassCustom s)     = "class(" ++ s ++ ")"

showCharLen :: CharacterLen -> String
showCharLen CharLenStar = "*"
showCharLen CharLenColon = ":"
showCharLen CharLenExp  = "*" -- FIXME, possibly, with a more robust const-exp
showCharLen (CharLenInt i) = show i

showDecl :: Declarator a -> String
showDecl (DeclArray _ _ e adims length' initial) =
  showExpr e ++
    "(" ++ aIntercalate "," showDim adims ++ ")" ++
    maybe "" (\e' -> "*" ++ showExpr e') length' ++
    maybe "" (\e' -> " = " ++ showExpr e') initial
showDecl (DeclVariable _ _ e length' initial) =
  showExpr e ++
    maybe "" (\e' -> "*" ++ showExpr e') length' ++
    maybe "" (\e' -> " = " ++ showExpr e') initial

showDim :: DimensionDeclarator a -> String
showDim (DimensionDeclarator _ _ me1 me2) = maybe "" ((++":") . showExpr) me1 ++ maybe "" showExpr me2

-- -----------------------------------------------------------------------------
-- * Analysis types to Graphviz DOT converters.

-- | Output a graph in the GraphViz DOT format
bbgrToDOT :: BBGr a -> String
bbgrToDOT = bbgrToDOT' IM.empty

-- | Output a supergraph in the GraphViz DOT format
superBBGrToDOT :: SuperBBGr a -> String
superBBGrToDOT sgr = bbgrToDOT' (superBBGrClusters sgr) (superBBGrGraph sgr)

-- shared code for DOT output
bbgrToDOT' :: IM.IntMap ProgramUnitName -> BBGr a -> String
bbgrToDOT' clusters' (BBGr{ bbgrGr = gr }) = execWriter $ do
  tell "strict digraph {\n"
  tell "node [shape=box,fontname=\"Courier New\"]\n"
  let entryNodes = filter (null . pre gr) (nodes gr)
  let nodes' = bfsn entryNodes gr
  _ <- forM nodes' $ \ n -> do
    let Just bs = lab gr n
    let mname = IM.lookup n clusters'
    case mname of Just name -> do tell $ "subgraph \"cluster " ++ showPUName name ++ "\" {\n"
                                  tell $ "label=\"" ++ showPUName name ++ "\"\n"
                                  tell "fontname=\"Courier New\"\nfontsize=24\n"
                  _         -> return ()
    tell $ "bb" ++ show n ++ "[label=\"" ++ show n ++ "\\l" ++ concatMap showBlock bs ++ "\"]\n"
    when (null bs) . tell $ "bb" ++ show n ++ "[shape=circle]\n"
    tell $ "bb" ++ show n ++ " -> {"
    _ <- forM (suc gr n) $ \ m -> tell (" bb" ++ show m)
    tell "}\n"
    when (isJust mname) $ tell "}\n"
  tell "}\n"

--------------------------------------------------------------------------------

aIntercalate :: [a1] -> (t a2 -> [a1]) -> AList t a2 -> [a1]
aIntercalate sep f = intercalate sep . map f . aStrip
