{-# LANGUAGE OverloadedStrings #-}

module Core where

import Control.Exception (throw)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except
import Data.Aeson
import Data.Group (Group, invert)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time.Clock.System (SystemTime, getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Typeable ()
import Data.Vector qualified as V
import Relude

newtype RemainingFailureTolerance = RemainingFailureTolerance Int
  deriving (Eq, Ord, Show, Num)

data OpenFile = OpenFile
  { openFileName :: Text,
    openFileContents :: Text
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON OpenFile

instance FromJSON OpenFile

renderOpenFile :: OpenFile -> Text
renderOpenFile (OpenFile name contents) = "{ openFileName: " <> name <> ", openFileContents:\n" <> contents <> "\n}"

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

addOpenFile :: Text -> Text -> AppState -> AppState
addOpenFile name contents theState =
  theState {stateOpenFiles = OpenFile name contents : stateOpenFiles theState}

updateOpenFile :: Text -> Text -> AppState -> AppState
updateOpenFile name contents theState =
  let upd x = x {openFileContents = contents}
   in theState {stateOpenFiles = map (\x -> if openFileName x /= name then x else upd x) (stateOpenFiles theState)}

ensureOpenFile :: Text -> Text -> AppState -> AppState
ensureOpenFile name contents theState =
  if any (\x -> openFileName x == name) (stateOpenFiles theState)
    then updateOpenFile name contents theState
    else addOpenFile name contents theState

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
    configForbiddenFiles :: [ForbiddenFile],
    configModelTemperature :: Maybe Float,
    configModelMaxInputTokens :: Int
  }
  deriving (Eq, Ord, Show)

data ProjectConfig = ProjectConfig {
  projectDependencyNames :: [Text],
  projectInitialFiles :: [(FilePath, Text)]
  } deriving (Eq, Ord, Show)

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
    [] ->
      if T.isInfixOf "/" name
        then Just $ "Filename " <> name <> " is forbidden because it contains '/'; no nested paths are allowed!"
        else Nothing

-- Metrics type for tracking
data Metrics = Metrics
  { metricsTokensIn :: Int,
    metricsTokensOut :: Int,
    metricsCost :: Double,
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
  (<>) (Metrics in1 out1 cost1 time1 timeComp1 timeTest1 syn1 comp1 test1) (Metrics in2 out2 cost2 time2 timeComp2 timeTest2 syn2 comp2 test2) =
    Metrics
      { metricsTokensIn = in1 + in2,
        metricsTokensOut = out1 + out2,
        metricsCost = cost1 + cost2,
        metricsApiTime = time1 + time2,
        metricsCompileTime = timeComp1 + timeComp2,
        metricsTestTime = timeTest1 + timeTest2,
        metricsNumSyntaxErrors = syn1 + syn2,
        metricsNumCompileFails = comp1 + comp2,
        metricsNumTestFails = test1 + test2
      }

instance Monoid Metrics where
  mempty = Metrics 0 0 0 0 0 0 0 0 0

instance Group Metrics where
  invert (Metrics tin tout cost time timeComp timeTest syn comp test) =
    Metrics
      { metricsTokensIn = -tin,
        metricsTokensOut = -tout,
        metricsCost = -cost,
        metricsApiTime = -time,
        metricsCompileTime = -timeComp,
        metricsTestTime = -timeTest,
        metricsNumSyntaxErrors = -syn,
        metricsNumCompileFails = -comp,
        metricsNumTestFails = -test
      }

data CompileTestState = CompileTestState {compileRes :: Maybe Text, testRes :: Maybe Text, numConsecutiveSyntaxCheckFails :: Int}
  deriving (Generic, Eq, Ord, Show)

instance FromJSON CompileTestState

instance ToJSON CompileTestState

data AppState = AppState
  { stateMetrics :: Metrics,
    stateOpenFiles :: [OpenFile],
    stateFiles :: [ExistingFile],
    stateCompileTestRes :: CompileTestState
  }
  deriving (Eq, Ord, Show)

updateStateMetrics :: Metrics -> AppState -> AppState
updateStateMetrics metrics st = st {stateMetrics = stateMetrics st <> metrics}

updateLastCompileState :: Maybe Text -> AppState -> AppState
updateLastCompileState res st =
  st {stateCompileTestRes = (stateCompileTestRes st) {compileRes = res}}

updateLastTestState :: Maybe Text -> AppState -> AppState
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

data MsgKind = CompileFailMsg | TestFailMsg | OtherMsg
  deriving (Eq, Ord, Show)

data Context = Context
  { contextBackground :: Text,
    contextTask :: Text,
    contextRest :: [(MsgKind, Message)]
  }
  deriving (Eq, Ord, Show)

addToContext :: Context -> MsgKind -> Message -> Context
addToContext c kind msg = c {contextRest = contextRest c ++ [(kind, msg)]}

updateContextMessages :: Context -> ([(MsgKind, Message)] -> [(MsgKind, Message)]) -> Context
updateContextMessages ctxt fn = ctxt {contextRest = fn (contextRest ctxt)}

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
