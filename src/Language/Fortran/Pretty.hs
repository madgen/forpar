{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Fortran.Pretty where

import Data.Char
import Prelude hiding (EQ,LT,GT)
import Language.Fortran.AST
import Language.Fortran.ParserMonad
import Language.Fortran.Util.Position
import Language.Fortran.Util.FirstParameter
import Language.Fortran.Util.SecondParameter

import Text.PrettyPrint

class Pretty t where
   pprint :: FortranVersion -> t -> Doc

-- A subset of Value permit the 'FirstParameter' operation
instance FirstParameter (Value a) String
instance (FirstParameter (Value a) String, Pretty (Expression a))
       => Pretty (Value a) where
    pprint v ValStar       = char '*'
    pprint v ValAssignment = text "assignment (=)"
    pprint v (ValComplex e1 e2) =
        parens $ commaSep [pprint v e1, pprint v e2]
    pprint v (ValString str) =
        char '"' <> text str <> char '"'
    pprint v valLit =
        text . getFirstParameter $ valLit

instance Pretty (Expression a) => Pretty (Index a) where
    pprint v (IxSingle _ s Nothing e) = pprint v e
    -- This is an intermediate expression form which shouldn't make it
    -- to the pretty printer
    pprint v (IxSingle _ s (Just _) e) = pprint v e
    pprint v (IxRange _ s low up stride) =
       low' <> char ':' <> up' <> stride'
      where
        low' = maybe empty (pprint v) low
        up'  = maybe empty (pprint v) up
        stride' =  maybe empty (\e -> char ':' <> pprint v e) stride

instance (Pretty (Expression a)) => Pretty (DoSpecification a) where
    pprint v (DoSpecification _ s e0assign en stride) =
      case e0assign of
        s@(StExpressionAssign {}) ->
          pprint v s <> comma <+> pprint v en
                     <> (maybe empty (\e -> comma <> pprint v e) stride)

        _ -> error $ "Malformed syntax tree: do-specification has non-assignment\
                      \statement at location " ++ show s

instance (Pretty (Argument a), Pretty (Value a)) => Pretty (Expression a) where
    pprint v (ExpValue _ s val)  =
         pprint v val

    pprint v (ExpBinary _ s op e1 e2) =
        floatDoc s $ pprint v e1 <+> pprint v op <+> pprint v e2

    pprint v (ExpUnary _ s op e) =
        floatDoc s $ pprint v op <+> pprint v e

    pprint v (ExpSubscript _ s e ixs) =
        floatDoc s $ pprint v e
                 <> parens (commaSep (map (pprint v) (aStrip ixs)))

    pprint v (ExpDataRef _ s e1 e2) =
        floatDoc s $ pprint v e1 <+> char '%' <+> pprint v e2

    pprint v (ExpFunctionCall _ s e mes) =
        floatDoc s $ pprint v e <> parens (maybe empty (pprint v) mes)

    pprint v (ExpImpliedDo _ s es dospec) =
        floatDoc s $ pprint v es <> comma <+> (pprint v dospec)

    pprint v (ExpInitialisation _ s es) =
        floatDoc s $ text "(/" <> pprint v es <> text "/)"

    pprint v (ExpReturnSpec _ s e) =
        floatDoc s $ char '*' <> pprint v e

instance (Pretty (e a)) => Pretty (AList e a) where
    pprint v es = commaSep (map (pprint v) (aStrip es))

instance Pretty (Argument a) where
    pprint v (Argument _ s key e) = floatDoc s $
       case key of
         Just keyName -> text keyName <+> char '=' <+> pprint v e
         Nothing      -> pprint v e

instance Pretty BaseType where
    pprint v TypeInteger = caps v "integer"
    pprint v TypeReal    = caps v "real"
    pprint v TypeDoublePrecision = caps v "double precision"
    pprint v TypeComplex = caps v "complex"
    pprint Fortran77Extended TypeDoubleComplex = text "DOUBLECOMPLEX"
    pprint v TypeLogical = caps v "logical"
    pprint v TypeCharacter = caps v "character"
    pprint v (TypeCustom str) = text "type(" <> text str <> text ")"

instance Pretty (TypeSpec a) where
    pprint v (TypeSpec _ a basetype mselector) =
      pprint v basetype <>
      case mselector of
        Nothing -> empty
        Just (Selector _ s Nothing Nothing) -> empty
        Just (Selector _ s mlength mkind)   ->
          case basetype of
            -- Character type spec
            TypeCharacter ->
              parens
                (maybe empty (\e -> text "len=" <> pprint v e) mlength
              <> maybe empty (\k -> comma <+> text "kind=" <> pprint v k) mkind)
            -- Non character type spec
            _ -> parens (maybe empty (\k -> text "kind=" <> pprint v k) mkind)

instance Pretty (DimensionDeclarator a) where
    pprint v (DimensionDeclarator _ _ me1 me2) =
      case (me1, me2) of
        (Nothing, Nothing) -> char ':'
        (Nothing, Just (ExpValue _ _ ValStar)) -> char '*'
        (me1, me2) -> pprint v me1 <> char ':' <> pprint v me2

instance Pretty a => Pretty (Maybe a) where
    pprint v Nothing  = empty
    pprint v (Just e) = pprint v e

instance Pretty Intent where
    pprint v In = caps v "in"
    pprint v Out = caps v "out"
    pprint v InOut = caps v "in out"

instance Pretty (Expression a) => Pretty (Declarator a) where
    pprint v (DeclVariable _ s e Nothing (Just e')) =
        pprint v e <+> char '=' <+> pprint v e'
    pprint v (DeclVariable _ s e Nothing Nothing) =
        pprint v e
    pprint v (DeclVariable _ s e (Just (ExpValue _ _ ValStar)) Nothing) =
        pprint v e <+> char '*' <+> parens (char '*')
    pprint v (DeclVariable _ s e (Just len) Nothing) =
        pprint v e <+> char '*' <+> pprint v len
    pprint v (DeclVariable _ s e (Just len) (Just e')) =
        error $ "Malformed syntax tree: decl-variable has both length and\
              \and initial value at " ++ show s
    pprint v (DeclArray _ s ae dims Nothing Nothing) =
        pprint v ae <> parens (pprint v dims)

    pprint v (DeclArray _ s ae dims (Just (ExpValue _ _ ValStar)) Nothing) =
        pprint v ae <> parens (pprint v dims) <> char '*' <> (parens (char '*'))

    pprint v (DeclArray _ s ae dims (Just len) Nothing) =
        pprint v ae <> parens (pprint v dims) <> char '*' <> (pprint v len)

    pprint v (DeclArray _ s ae dims len init) =
        error $ "Malformed syntax tree: decl-array has init value at " ++ show s


instance Pretty (DimensionDeclarator a) => Pretty (Attribute a) where
    pprint v (AttrParameter _ _)   = caps v "parameter"
    pprint v (AttrPublic    _ _)   = caps v "public"
    pprint v (AttrPrivate   _ _)   = caps v "private"
    pprint v (AttrAllocatable _ _) = caps v "allocatable"
    pprint v (AttrDimension _ _ dims) = caps v "dimesion" <> parens (pprint v dims)
    pprint v (AttrExternal _ _)    = caps v "external"
    pprint v (AttrIntent _ _ i)    = caps v "intent" <> parens (pprint v i)
    pprint v (AttrIntrinsic _ _)   = caps v "intrinsic"
    pprint v (AttrOptional _ _)    = caps v "optional"
    pprint v (AttrPointer _ _)     = caps v "pointer"
    pprint v (AttrSave _ _)        = caps v "save"
    pprint v (AttrTarget _ _)      = caps v "target"

instance Pretty (Expression a) => Pretty (Statement a) where
    pprint v (StDeclaration _ s typespec attributes declarators) = empty
    pprint v (StExpressionAssign _ span e1 e2) = empty
    pprint v _ = empty

instance Pretty BinaryOp where
    pprint v Addition       = char '+'
    pprint v Subtraction    = char '-'
    pprint v Multiplication = char '*'
    pprint v Division       = char '/'
    pprint v Exponentiation = text "**"
    pprint v Concatenation  = text "//"
    pprint v GT  = text $ if v77orLess v then ".gt." else ">"
    pprint v LT  = text $ if v77orLess v then ".lt." else "<"
    pprint v LTE = text $ if v77orLess v then ".le." else "<="
    pprint v GTE = text $ if v77orLess v then ".ge." else ">="
    pprint v EQ  = text $ if v77orLess v then ".eq." else "=="
    pprint v NE  = text $ if v77orLess v then ".ne." else "!="
    pprint v Or  = text $ ".or."
    pprint v And = text $ ".and."
    pprint v Equivalent = text $ ".eqv."
    pprint v NotEquivalent = text $ ".neqv."
    pprint v (BinCustom custom) = text $ "." ++ custom ++ "."

instance Pretty UnaryOp where
    pprint v Plus  = char '+'
    pprint v Minus = char '-'
    pprint v Not   = text ".not."
    pprint v (UnCustom custom) = text $ "." ++ custom ++ "."

commaSep :: [Doc] -> Doc
commaSep = vcat . punctuate (comma <> space)

floatDoc :: SrcSpan -> Doc -> Doc
floatDoc span d | lineDistance span == 0 =
   -- If the rendered pretty print is less than the width of
   -- the span, then pad to the end with spaces
   if (length (render d)) < (columnDistance span)
   then vcat (d : (replicate ((columnDistance span) - (length (render d))) space))
   else d

-- Difficult to know what to dif line distance is non-zero
floatDoc span d | otherwise = d

caps v str | v77orLess v = text $ map toUpper str
           | otherwise   = text $ str