{-# LANGUAGE OverloadedStrings #-}

module Core where

import Control.Exception (throw)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Data.Aeson
import Data.Group (Group, invert)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Clock.System (SystemTime, getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Typeable ()
import qualified Text.Show
import Data.Vector qualified as V
import Relude

-- Get current time as POSIXTime (seconds since Unix epoch)
getCurrentPOSIXTime :: IO POSIXTime
getCurrentPOSIXTime = getPOSIXTime

data SummariseActionArg = SummariseActionArg{
  actionSummary :: Text,
  actionReason :: Text,
  actionFuturePlanSummary :: Text
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON SummariseActionArg
instance FromJSON SummariseActionArg

data PotentiallyBigMessage = PotentiallyBigMessage Text
  deriving (Generic, Eq, Ord)

instance Show PotentiallyBigMessage where
  show _ = "(Not shown to save space)"

newtype NumFailedTests = NumFailedTests Int
  deriving (Generic, Eq, Ord, Show, Num)
instance ToJSON NumFailedTests
instance FromJSON NumFailedTests

data EventResult = Succeeded | Failed | FailedWithError Text | FailedWithPotentiallyVeryLongError PotentiallyBigMessage
  deriving (Generic, Eq, Ord, Show)

data TracedEvent =
  EvtAiAction SummariseActionArg |
  EvtOpenFile Text EventResult | 
  EvtFocusFile Text EventResult |
  EvtCloseFile Text EventResult |
  EvtFileOp Text EventResult |
  EvtCompileProject EventResult |
  EvtTestProject EventResult NumFailedTests | 
  EvtAddDependency Text EventResult |
  EvtApproachCorrection Text |
  EvtEscalation Text Text
  deriving (Generic, Eq, Ord, Show)

data RemainingFailureTolerance = RemainingFailureTolerance
  {
    remainingSyntaxErrorTolerance :: Int,
    remainingSemanticErrorTolerance :: Int
  }
  deriving (Generic, Eq, Ord, Show)

instance FromJSON RemainingFailureTolerance
instance ToJSON RemainingFailureTolerance

addSyntaxError :: RemainingFailureTolerance -> RemainingFailureTolerance
addSyntaxError x = x {remainingSyntaxErrorTolerance = remainingSyntaxErrorTolerance x - 1}

-- Note: on a semantic error, we reset the syntax error count, as a semantic error
-- implies that the syntax was correct.
addSemanticError :: RemainingFailureTolerance -> Config -> RemainingFailureTolerance
addSemanticError x cfg = do
  let originalMaxSyntaxErrors = remainingSyntaxErrorTolerance $ configTaskMaxFailures cfg
  x {remainingSemanticErrorTolerance = remainingSemanticErrorTolerance x - 1,
     remainingSyntaxErrorTolerance = originalMaxSyntaxErrors}

failureToleranceExceeded :: RemainingFailureTolerance -> Bool
failureToleranceExceeded (RemainingFailureTolerance syn sem) = syn < 0 || sem < 0

instance Semigroup RemainingFailureTolerance where
  (<>) = (\(RemainingFailureTolerance syn1 sem1) (RemainingFailureTolerance syn2 sem2) -> RemainingFailureTolerance (syn1 + syn2) (sem1 + sem2))

instance Monoid RemainingFailureTolerance where
  mempty = RemainingFailureTolerance 0 0

data OpenFile = OpenFile
  { openFileName :: Text,
    openFileContents :: Text,
    openFileUnfocusedContents :: Text,
    openFileFocused :: Bool,
    openFileLastModified :: POSIXTime
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON OpenFile

instance FromJSON OpenFile

data FileFocused = FileFocusedTrue | FileFocusedFalse
  deriving (Eq, Ord, Show)

renderOpenFile :: (FileFocused -> Text -> AppM Text) -> OpenFile -> AppM Text
renderOpenFile modifier (OpenFile name contents unfocusedContents focused _) = do
  modified <- modifier (if focused then FileFocusedTrue else FileFocusedFalse) (if focused then contents else unfocusedContents)
  pure $ "{ openFileName: " <> name <> ", focused: " <> show focused <> ", openFileContents:\n" <> modified <> "\n}"

data ExistingFile = ExistingFile
  { existingFileName :: Text,
    existingFileDesc :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON ExistingFile

instance FromJSON ExistingFile

renderExistingFile :: ExistingFile -> Text
renderExistingFile (ExistingFile name description) = "{ existingFileName: " <> name <> ", Description: " <> description <> "}"

fileExists :: Text -> AppState -> Bool
fileExists name theState =
  any (\file -> existingFileName file == name) (stateFiles theState)

getOpenFile :: Text -> AppState -> Maybe OpenFile
getOpenFile name theState =
  find (\file -> openFileName file == name) (stateOpenFiles theState)

fileAlreadyOpen :: Text -> AppState -> Bool
fileAlreadyOpen name theState =
  any (\file -> openFileName file == name) (stateOpenFiles theState)

addExistingFile :: Text -> Text -> AppState -> AppState
addExistingFile name desc theState =
  theState {stateFiles = ExistingFile name desc : stateFiles theState}

getExistingFiles :: [Text] -> AppState -> Either Text [ExistingFile]
getExistingFiles filesToGet st =
  case errors of
    (err : _) -> Left err
    [] -> Right matches
  where
    existing = stateFiles st
    lookupFile name =
      case filter (\file -> existingFileName file == name) existing of
        [] -> Left $ "Error, file does not exist: " <> name
        (m : _) -> Right m

    results = map lookupFile filesToGet
    errors = lefts results
    matches = rights results

addOpenFile :: Text -> Text -> Text -> AppState -> AppState
addOpenFile name contents unfocusedContents theState =
  theState {stateOpenFiles = OpenFile name contents unfocusedContents False 0 : stateOpenFiles theState}

updateOpenFile :: Text -> Text -> AppState -> AppState
updateOpenFile name contents theState =
  let upd x = x {openFileContents = contents}
   in theState {stateOpenFiles = map (\x -> if openFileName x /= name then x else upd x) (stateOpenFiles theState)}

ensureOpenFile :: Text -> Text -> Text -> AppState -> AppState
ensureOpenFile name contents unfocusedContents theState =
  if any (\x -> openFileName x == name) (stateOpenFiles theState)
    then updateOpenFile name contents theState
    else addOpenFile name contents unfocusedContents theState

closeOpenFile :: Text -> AppState -> AppState
closeOpenFile name theState =
  theState {stateOpenFiles = filter (\x -> openFileName x /= name) (stateOpenFiles theState)}

updateExistingFilesRaw :: [Text] -> [ExistingFile] -> [ExistingFile]
updateExistingFilesRaw fileNames existingFiles =
  -- Keep existing files that are in fileNames, maintaining their descriptions
  [ef | ef <- existingFiles, existingFileName ef `elem` fileNames]
    ++
    -- Add new files with empty descriptions
    [ExistingFile fn "" | fn <- fileNames, fn `notElem` existingFileNames]
  where
    existingFileNames = map existingFileName existingFiles

updateExistingFiles :: [Text] -> AppState -> AppState
updateExistingFiles fileNames st = st {stateFiles = updateExistingFilesRaw fileNames (stateFiles st)}

updateFileDesc :: Text -> Text -> AppState -> AppState
updateFileDesc name desc st = st {stateFiles = updatedFiles}
  where
    updatedFiles = case L.findIndex ((== name) . existingFileName) (stateFiles st) of
      Just idx ->
        [ if i == idx then ExistingFile name desc else file
        | (i, file) <- zip [0 ..] (stateFiles st)
        ]
      Nothing -> stateFiles st ++ [ExistingFile name desc]

updateFileLastModified :: Text -> POSIXTime -> AppState -> AppState
updateFileLastModified fileName newTimestamp st =
  let updatedOpenFiles =
        map
          ( \file ->
              if openFileName file == fileName
                then file {openFileLastModified = newTimestamp}
                else file
          )
          (stateOpenFiles st)
   in st {stateOpenFiles = updatedOpenFiles}

data ForbiddenFile = ForbiddenFile
  { forbiddenFileName :: Text,
    forbiddenFileReason :: Text
  }
  deriving (Eq, Ord, Show)

data Config = Config
  { configApiKey :: Text,
    configApiSite :: Text,
    configLowIntModel :: Text,
    configMediumIntModel :: Text,
    configHighIntModel :: Text,
    configBaseDir :: FilePath,
    configCacheDir :: FilePath,
    configBuildTimeoutSeconds :: Int,
    configBuildNumJobs :: Int,
    configEnvVars :: [(Text, Text)],
    configGitUserName :: Text,
    configGitUserEmail :: Text,
    configTaskMaxFailures :: RemainingFailureTolerance,
    configRejectInvalidSyntaxDiffs :: Bool,
    configForbiddenFiles :: [ForbiddenFile],
    configMaxNumFocusedFiles :: Int,
    configModelTemperature :: Maybe Float,
    configModelMaxInputTokens :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Config where
  a <> b =
    Config
      { configApiKey = configApiKey b <> configApiKey a,
        configApiSite = configApiSite b <> configApiSite a,
        configLowIntModel = configLowIntModel b <> configLowIntModel a,
        configMediumIntModel = configMediumIntModel b <> configMediumIntModel a,
        configHighIntModel = configHighIntModel b <> configHighIntModel a,
        configBaseDir = configBaseDir b <> configBaseDir a,
        configCacheDir = configCacheDir b <> configCacheDir a,
        configBuildTimeoutSeconds = configBuildTimeoutSeconds b,
        configBuildNumJobs = configBuildNumJobs b,
        configEnvVars = configEnvVars a <> configEnvVars b,
        configGitUserName = configGitUserName b <> configGitUserName a,
        configGitUserEmail = configGitUserEmail b <> configGitUserEmail a,
        configTaskMaxFailures = configTaskMaxFailures a <> configTaskMaxFailures b,
        configRejectInvalidSyntaxDiffs = configRejectInvalidSyntaxDiffs a && configRejectInvalidSyntaxDiffs b,
        configForbiddenFiles = configForbiddenFiles a <> configForbiddenFiles b,
        configMaxNumFocusedFiles = configMaxNumFocusedFiles b,
        configModelTemperature = configModelTemperature b <|> configModelTemperature a,
        configModelMaxInputTokens = configModelMaxInputTokens b
      }

instance Monoid Config where
  mempty =
    Config
      { configApiKey = T.empty,
        configApiSite = T.empty,
        configLowIntModel = T.empty,
        configMediumIntModel = T.empty,
        configHighIntModel = T.empty,
        configBaseDir = "",
        configCacheDir = "",
        configBuildTimeoutSeconds = 0,
        configBuildNumJobs = 0,
        configEnvVars = [],
        configGitUserName = T.empty,
        configGitUserEmail = T.empty,
        configTaskMaxFailures = mempty,
        configRejectInvalidSyntaxDiffs = False,
        configForbiddenFiles = [],
        configMaxNumFocusedFiles = 0,
        configModelTemperature = Nothing,
        configModelMaxInputTokens = 0
      }

data ProjectConfig = ProjectConfig
  { projectDependencyNames :: [Text],
    projectInitialFiles :: [(FilePath, Text)]
  }
  deriving (Eq, Ord, Show)

data IntelligenceRequired = HighIntelligenceRequired | MediumIntelligenceRequired | LowIntelligenceRequired
  deriving (Eq, Ord, Show)

getModel :: Config -> IntelligenceRequired -> Text
getModel cfg LowIntelligenceRequired = configLowIntModel cfg
getModel cfg MediumIntelligenceRequired = configMediumIntModel cfg
getModel cfg HighIntelligenceRequired = configHighIntModel cfg

isFileForbidden :: Config -> Text -> Maybe Text
isFileForbidden cfg name = do
  let match = filter (\x -> forbiddenFileName x == name) (configForbiddenFiles cfg)
  case match of
    (x : _) -> Just $ "File " <> name <> " is forbidden to modify because: " <> forbiddenFileReason x
    [] -> Nothing

--      if T.isInfixOf "/" name
--        then Just $ "Filename " <> name <> " is forbidden because it contains '/'; no nested paths are allowed!"
--        else Nothing

data ModelTokenMetrics = ModelTokenMetrics
  { tokensIn :: Int,
    tokensOut :: Int,
    tokensCost :: Double
  }
  deriving (Generic, Eq, Ord, Show)

instance Semigroup ModelTokenMetrics where
  (<>) (ModelTokenMetrics in1 out1 cost1) (ModelTokenMetrics in2 out2 cost2) =
    ModelTokenMetrics
      { tokensIn = in1 + in2,
        tokensOut = out1 + out2,
        tokensCost = cost1 + cost2
      }

instance Monoid ModelTokenMetrics where
  mempty = ModelTokenMetrics 0 0 0

instance Group ModelTokenMetrics where
  invert (ModelTokenMetrics tin tout tcost) =
    ModelTokenMetrics
      { tokensIn = -tin,
        tokensOut = -tout,
        tokensCost = -tcost
      }

instance ToJSON ModelTokenMetrics
instance FromJSON ModelTokenMetrics

-- Metrics type for tracking
data Metrics = Metrics
  { metricsLowInt :: ModelTokenMetrics,
    metricsMediumInt :: ModelTokenMetrics,
    metricsHighInt :: ModelTokenMetrics,
    metricsApiTime :: Int64,
    metricsCompileTime :: Int64,
    metricsTestTime :: Int64,
    metricsNumSyntaxErrors :: Int64,
    metricsNumCompileFails :: Int64,
    metricsNumTestFails :: Int64
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Metrics
instance FromJSON Metrics

instance Semigroup Metrics where
  (<>) (Metrics lowInt1 medInt1 highInt1 time1 timeComp1 timeTest1 syn1 comp1 test1) (Metrics lowInt2 medInt2 highInt2 time2 timeComp2 timeTest2 syn2 comp2 test2) =
    Metrics
      { metricsLowInt = lowInt1 <> lowInt2,
        metricsMediumInt = medInt1 <> medInt2,
        metricsHighInt = highInt1 <> highInt2,
        metricsApiTime = time1 + time2,
        metricsCompileTime = timeComp1 + timeComp2,
        metricsTestTime = timeTest1 + timeTest2,
        metricsNumSyntaxErrors = syn1 + syn2,
        metricsNumCompileFails = comp1 + comp2,
        metricsNumTestFails = test1 + test2
      }

instance Monoid Metrics where
  mempty = Metrics mempty mempty mempty 0 0 0 0 0 0

instance Group Metrics where
  invert (Metrics lowInt medInt highInt time timeComp timeTest syn comp test) =
    Metrics
      { metricsLowInt = invert lowInt,
        metricsMediumInt = invert medInt,
        metricsHighInt = invert highInt,
        metricsApiTime = -time,
        metricsCompileTime = -timeComp,
        metricsTestTime = -timeTest,
        metricsNumSyntaxErrors = -syn,
        metricsNumCompileFails = -comp,
        metricsNumTestFails = -test
      }

data CompileTestState = CompileTestState {compileRes :: Maybe Text, testRes :: Maybe (Text, NumFailedTests), numConsecutiveCompilationFails :: Int, numConsecutiveSyntaxCheckFails :: Int}
  deriving (Generic, Eq, Ord, Show)

instance FromJSON CompileTestState

instance ToJSON CompileTestState

instance Semigroup CompileTestState where
  a <> b =
    CompileTestState
      (compileRes a <|> compileRes b)
      (testRes a <|> testRes b)
      (numConsecutiveCompilationFails a + numConsecutiveCompilationFails b)
      (numConsecutiveSyntaxCheckFails a + numConsecutiveSyntaxCheckFails b)

instance Monoid CompileTestState where
  mempty = CompileTestState Nothing Nothing 0 0

data AppState = AppState
  { stateMetrics :: Metrics,
    stateOpenFiles :: [OpenFile],
    stateFiles :: [ExistingFile],
    stateCompileTestRes :: CompileTestState
  }
  deriving (Eq, Ord, Show)

instance Semigroup AppState where
  a <> b =
    AppState
      (stateMetrics a <> stateMetrics b)
      (stateOpenFiles a <> stateOpenFiles b)
      (stateFiles a <> stateFiles b)
      (stateCompileTestRes a <> stateCompileTestRes b)

instance Monoid AppState where
  mempty = AppState mempty [] [] mempty

updateStateMetrics :: Metrics -> AppState -> AppState
updateStateMetrics metrics st = st {stateMetrics = stateMetrics st <> metrics}

updateLastCompileState :: Maybe Text -> AppState -> AppState
updateLastCompileState res st =
  st {stateCompileTestRes = (stateCompileTestRes st) {compileRes = res, numConsecutiveCompilationFails = if isJust res then (numConsecutiveCompilationFails (stateCompileTestRes st) + 1) else 0}}

updateLastTestState :: Maybe (Text, NumFailedTests) -> AppState -> AppState
updateLastTestState res st =
  st {stateCompileTestRes = (stateCompileTestRes st) {testRes = res}}

onSyntaxCheckFail :: AppState -> AppState
onSyntaxCheckFail st =
  st {stateCompileTestRes = (stateCompileTestRes st) {numConsecutiveSyntaxCheckFails = numConsecutiveSyntaxCheckFails (stateCompileTestRes st) + 1}}

onSyntaxCheckPass :: AppState -> AppState
onSyntaxCheckPass st =
  st {stateCompileTestRes = (stateCompileTestRes st) {numConsecutiveSyntaxCheckFails = 0}}

resetCompileTestState :: AppM ()
resetCompileTestState = do
  modify' $ updateLastCompileState Nothing
  modify' $ updateLastTestState Nothing

clearOpenFiles :: AppState -> AppState
clearOpenFiles st = st {stateOpenFiles = []}

type AppError = Text

newtype AppM a = AppM
  { runAppM :: ReaderT Config (StateT AppState (ExceptT AppError IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadState AppState,
      MonadError AppError,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

getEnvVars :: AppM [(String, String)]
getEnvVars = do
  cfg <- ask
  pure $ map (bimap T.unpack T.unpack) $ configEnvVars cfg

runApp :: Config -> AppState -> AppM a -> IO (Either AppError (a, AppState))
runApp config appState app =
  runExceptT
    $ runStateT
      (runReaderT (runAppM app) config)
      appState

data Message = Message
  { role :: Text,
    content :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Message

instance FromJSON Message

renderMessage :: Message -> Text
renderMessage (Message role text) = "{" <> role <> ": \n" <> text <> "\n}"

data MsgKind = CompileFailMsg | TestFailMsg | FileModifiedMsg Text | FileClosedMsg Text | OtherMsg
  deriving (Eq, Ord, Show)

data Context = Context
  { contextBackground :: Text,
    contextTask :: Text,
    contextRest :: [(MsgKind, Message)],
    contextNumErrors :: Int,
    contextEvents :: [TracedEvent]
  }
  deriving (Eq, Ord, Show)

makeBaseContext :: Text -> Text -> Context
makeBaseContext background task = Context {contextBackground = background, contextTask = task, contextRest = [], contextNumErrors = 0, contextEvents = []}

addToContext :: Context -> MsgKind -> Message -> Context
addToContext c kind msg = c {contextRest = contextRest c ++ [(kind, msg)]}

addEvtToContext :: Context -> TracedEvent -> Context
addEvtToContext c evt = c {contextEvents = contextEvents c ++ [evt]}

addEvtsToContext :: Context -> [TracedEvent] -> Context
addEvtsToContext c evts = c {contextEvents = contextEvents c ++ evts}

contextRecordError :: Context -> Context
contextRecordError ctxt = ctxt {contextNumErrors = contextNumErrors ctxt}

updateContextMessages :: Context -> ([(MsgKind, Message)] -> [(MsgKind, Message)]) -> Context
updateContextMessages ctxt fn = ctxt {contextRest = fn (contextRest ctxt)}

hasFileBeenModified :: Context -> Text -> Bool
hasFileBeenModified context fileName =
  any isFileModified (contextRest context)
  where
    isFileModified (FileModifiedMsg name, _) = name == fileName
    isFileModified _ = False

hasAnyFileBeenClosed :: Context -> Bool
hasAnyFileBeenClosed context =
  any isFileClosed (contextRest context)
  where
    isFileClosed (FileClosedMsg _, _) = True
    isFileClosed _ = False

data Role = RoleSystem | RoleAssistant | RoleUser
  deriving (Show, Eq, Ord)

roleName :: Role -> Text
roleName x = T.toLower $ T.drop 4 $ show x

roleUser :: Text
roleUser = roleName RoleUser

roleAssistant :: Text
roleAssistant = roleName RoleAssistant

addToContextUser :: Context -> MsgKind -> Text -> Context
addToContextUser ctxt kind txt = addToContext ctxt kind Message {role = roleUser, content = txt}

addToContextAi :: Context -> MsgKind -> Text -> Context
addToContextAi ctxt kind txt = addToContext ctxt kind Message {role = roleAssistant, content = txt}

focusFileInner :: Text -> Int -> AppState -> AppState
focusFileInner fileName maxNumOpenFiles st =
  let -- Check if the specified file exists in the open files
      fileAlreadyExists = any (\file -> openFileName file == fileName) (stateOpenFiles st)

      -- Only proceed with changes if the file exists
      updatedOpenFiles =
        if fileAlreadyExists
          then
            -- Update the focus state for the specified file
            map
              ( \file ->
                  if openFileName file == fileName
                    then file {openFileFocused = True}
                    else file
              )
              (stateOpenFiles st)
          else
            -- Return unchanged if the file doesn't exist
            stateOpenFiles st

      -- Count how many files are focused after the initial update
      numFocused = length $ filter openFileFocused updatedOpenFiles

      -- Only consider unfocusing if we actually made changes and exceeded the limit
      finalOpenFiles =
        if fileAlreadyExists && numFocused > maxNumOpenFiles
          then unfocusOldestFile updatedOpenFiles
          else updatedOpenFiles
   in st {stateOpenFiles = finalOpenFiles}

-- Helper function to unfocus the oldest file among the focused ones
unfocusOldestFile :: [OpenFile] -> [OpenFile]
unfocusOldestFile files =
  let -- Get only the focused files
      focusedFiles = filter openFileFocused files

      -- Find the file with the oldest lastModified timestamp among focused files
      -- Use safe pattern matching in case the list is empty (shouldn't happen based on our logic)
      oldestFile = case focusedFiles of
        [] -> error "No focused files found" -- This shouldn't occur given our checks
        _ -> L.minimumBy (comparing openFileLastModified) focusedFiles
   in -- Update the list, setting openFileFocused to False for the oldest file
      map
        ( \file ->
            if openFileName file == openFileName oldestFile
              then file {openFileFocused = False}
              else file
        )
        files

focusFile :: Text -> AppM ()
focusFile fileName = do
  cfg <- ask
  modify' $ focusFileInner fileName (configMaxNumFocusedFiles cfg)

fileFocused :: Text -> AppState -> Bool
fileFocused fileName st =
  any (\file -> openFileName file == fileName && openFileFocused file) (stateOpenFiles st)

focusedFileNames :: AppState -> [Text]
focusedFileNames st =
  map openFileName $ filter (\file -> openFileFocused file) (stateOpenFiles st)

data TimeOverflowException = TimeOverflowException
  deriving (Show, Typeable)

instance Exception TimeOverflowException

getDuration :: SystemTime -> SystemTime -> Int64
getDuration start end = do
  let startNano = systemToTAITime start
  let endNano = systemToTAITime end
  let diffNano = floor $ diffAbsoluteTime endNano startNano * 1_000_000_000 :: Integer
  if diffNano
    > fromIntegral (maxBound :: Int64)
    || diffNano
    < fromIntegral (minBound :: Int64)
    then throw TimeOverflowException diffNano
    else fromIntegral diffNano

timeIONano64 :: IO a -> IO (a, Int64)
timeIONano64 action = do
  start <- getSystemTime
  result <- action
  end <- getSystemTime
  return (result, getDuration start end)

timeIONano64M :: AppM a -> AppM (a, Int64)
timeIONano64M action = do
  start <- liftIO getSystemTime
  result <- action
  end <- liftIO getSystemTime
  return (result, getDuration start end)

foldAllEithers :: (a -> AppM (Either Text b)) -> [a] -> AppM (Either Text [b])
foldAllEithers f as = do
  results <- traverse f as
  return $ case partitionEithers results of
    ([], bs) -> Right bs
    (errs, _) -> Left (T.intercalate "\n" errs)

foldWithErrors :: (Foldable t, Traversable t) => (a -> IO (Either Text ())) -> t a -> IO (Either Text ())
foldWithErrors f xs = do
  results <- mapM f xs
  return $ foldr combineResults (Right ()) results
  where
    combineResults :: Either Text () -> Either Text () -> Either Text ()
    combineResults (Left err1) (Left err2) = Left (err1 <> "\n" <> err2)
    combineResults (Left err) _ = Left err
    combineResults _ (Left err) = Left err
    combineResults _ _ = Right ()

isDocFileExtension :: Text -> Bool
isDocFileExtension fileName = ".txt" `T.isSuffixOf` fileName || ".doc" `T.isSuffixOf` fileName || ".md" `T.isSuffixOf` fileName

lookupText :: Text -> [(Text, Text)] -> Maybe Text
lookupText key dict = snd <$> L.find (\(k, _) -> k == key) dict

-- | Truncate a text by keeping only the first and last N lines
-- and adding a skipping message in between.
truncateText :: Int -> Text -> Text
truncateText n input
  | n <= 0 = input -- Handle non-positive n values
  | totalLines <= 2 * n = input -- No truncation needed
  | otherwise = result
  where
    -- Split the input text into lines
    linesVector = V.fromList $ T.lines input
    totalLines = V.length linesVector

    -- Get the first and last N lines
    firstNLines = V.slice 0 n linesVector
    lastNLines = V.slice (totalLines - n) n linesVector

    -- Create the skipping message
    skippingMsg = T.pack $ "... skipping " ++ show (totalLines - (2 * n)) ++ " lines ..."

    -- Combine everything into a single Text
    result =
      T.intercalate "\n"
        $ V.toList firstNLines
        ++ [skippingMsg]
        ++ V.toList lastNLines

data FileChangeBounds = FileChangeBounds Int Int
  deriving (Eq, Ord, Show)

sliceList :: Int -> Int -> [a] -> [a]
sliceList n m xs = take (m - n + 1) (drop n xs)

eitherM :: (Monad m) => (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM f _ (Left a) = f a
eitherM _ g (Right b) = g b

runActionWithoutModifyingState :: forall a. AppM a -> AppM a
runActionWithoutModifyingState actionToRun = do
    currentState <- get
    currentConfig <- ask

    --    Unwrap AppM -> ReaderT -> StateT -> ExceptT -> IO
    --    Run the ReaderT layer using the captured config
    let computationToRun :: StateT AppState (ExceptT AppError IO) a
        computationToRun = runReaderT (runAppM actionToRun) currentConfig

    --    This runs the computation starting with 'currentState' and gives
    --    us the result within the underlying monad (ExceptT AppError IO),
    --    discarding the final state of the *inner* computation.
    let underlyingComputation :: ExceptT AppError IO a
        underlyingComputation = evalStateT computationToRun currentState

    -- 4. Run the underlying ExceptT/IO stack
    resultOrError <- liftIO $ runExceptT underlyingComputation

    -- 5. Handle the result within the *outer* AppM context
    case resultOrError of
        Left err     -> throwError err 
        Right result -> pure result    
