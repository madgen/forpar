module Forpar.Transformation.TransformMonad ( getTypes
                                            , queryIDType
                                            , getProgramFile
                                            , putProgramFile
                                            , modifyProgramFile
                                            , renameProgramFile
                                            , unrenameProgramFile
                                            , runTransform
                                            , Transform(..) ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Data.Map (lookup, Map)

import Forpar.Analysis.Types
import Forpar.Analysis
import Forpar.Analysis.Renaming
import Forpar.AST (ProgramFile)

data TransformationState = TransformationState 
  { transProgramFile :: ProgramFile ()
  , transTypes :: Maybe (Map TypeScope (Map String IDType))
  , transUnrenameMap :: Maybe NameMap }

type Transform = State TransformationState

runTransform :: Transform () -> ProgramFile () -> ProgramFile ()
runTransform trans pf = transProgramFile . execState trans $ initState
  where
    initState = TransformationState 
      { transProgramFile = pf 
      , transTypes = Nothing
      , transUnrenameMap = Nothing }

getProgramFile :: Transform (ProgramFile ())
getProgramFile = gets transProgramFile

putProgramFile :: ProgramFile () -> Transform ()
putProgramFile pf = do
  state <- get
  put $ state { transProgramFile = pf }

modifyProgramFile :: (ProgramFile () -> ProgramFile ()) -> Transform ()
modifyProgramFile f = do
  state <- get
  put $ state { transProgramFile = f (transProgramFile state) }

renameProgramFile :: Transform ()
renameProgramFile = do
  pf <- getProgramFile
  let (pf', nm) = renameAndStrip . analyseRenames . initAnalysis $ pf
  modify $ \ s -> s { transUnrenameMap = Just nm, transProgramFile = pf', transTypes = Nothing }

unrenameProgramFile :: Transform ()
unrenameProgramFile = do
  pf <- getProgramFile
  m_nm <- gets transUnrenameMap
  case m_nm of
    Just nm -> modify $ \ s -> s { transUnrenameMap = Nothing
                                 , transProgramFile = unrename (pf, nm), transTypes = Nothing }
    Nothing -> return ()

-- If types are requested and are not available automatically infer them.
getTypes :: Transform (Map TypeScope (Map String IDType))
getTypes = do
  mTypes <- gets transTypes
  case mTypes of
    Just m -> return m
    Nothing -> do
      state <- get
      let pf = transProgramFile state
      let typeMapping = inferTypes pf
      put $ state { transTypes = Just typeMapping }
      return typeMapping

queryIDType :: TypeScope -> String -> Map TypeScope (Map String IDType) -> Maybe IDType
queryIDType ts s mapping = do
  innerMapping <- lookup ts mapping
  lookup s innerMapping

queryIDTypeM :: TypeScope -> String -> Transform (Maybe IDType)
queryIDTypeM ts s = do
  mapping <- getTypes
  return $ queryIDType ts s mapping