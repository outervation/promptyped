{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OpenRouterClient (sendQuery, QueryResult (..), GenerationStats (..), UsageData (..)) where

import Control.Exception (Handler (..), IOException, catches)
import Control.Monad
import Core
import Data.Aeson
import Data.Aeson.KeyMap qualified as HM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Default.Class (def)
import Data.List as L
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Conc (threadDelay)
import Logging qualified
import Network.HTTP.Client
  ( BodyReader,
    HttpException,
    ManagerSettings (..),
    RequestBody (..),
    Response (..),
    brConsume,
    brRead,
    method,
    parseRequest,
    rawConnectionModifySocket,
    requestBody,
    requestHeaders,
    responseStatus,
    responseTimeout,
    responseTimeoutMicro,
    responseVersion,
    withResponse,
  )
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Types (Status (..))
import Network.Socket (SocketOption (..), setSocketOption)
import Relude hiding (id)
import Servant.API
import Servant.Client (BaseUrl (..), ClientError (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import Servant.Client.Core as SCC (RequestF (..), ResponseF (..), responseHttpVersion)

data StreamIt = StreamIt | DontStreamIt deriving (Eq, Ord, Show)

data ChatRequest = ChatRequest
  { model :: Text,
    messages :: [Message],
    stream :: Maybe Bool,
    temperature :: Maybe Float
  }
  deriving (Generic, Show)

instance ToJSON ChatRequest

data UsageData = UsageData
  { prompt_tokens :: Int,
    completion_tokens :: Int,
    total_tokens :: Int
  }
  deriving (Generic, Show)

instance FromJSON UsageData

data ChatResponse = ChatResponse
  { id :: Text,
    choices :: [Choice],
    usage :: UsageData
  }
  deriving (Generic, Show)

instance FromJSON ChatResponse

data GenerationStats = GenerationStats
  { tokens_prompt :: Int,
    tokens_completion :: Int,
    total_cost :: Double
  }
  deriving (Generic, Show)

instance FromJSON GenerationStats

data GenerationResponse = GenerationResponse
  { generationData :: GenerationStats
  }
  deriving (Generic, Show)

instance FromJSON GenerationResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "generationData" -> "data"
            x -> x
        }

data Choice = Choice
  { message :: Message
  }
  deriving (Generic, Show)

instance FromJSON Choice

data QueryResult = QueryResult
  { message :: Message,
    usage :: UsageData,
    stats :: GenerationStats
  }
  deriving (Generic, Show)

type ChatCompletionsAPI =
  "chat"
    :> "completions"
    :> Header "Authorization" Text
    :> Header "HTTP-Referer" Text
    :> Header "X-Title" Text
    :> ReqBody '[JSON] ChatRequest
    :> Post '[JSON] ChatResponse

type GenerationAPI =
  "generation"
    :> QueryParam "id" Text
    :> Header "Authorization" Text
    :> Get '[JSON] GenerationResponse

-- Combined base API with both endpoints
type BaseAPI = ChatCompletionsAPI :<|> GenerationAPI

-- Two variants of the full API
type DirectAPI = "v1" :> BaseAPI

type PrefixedAPI = "api" :> "v1" :> BaseAPI

-- Create separate proxies
directAPI :: Proxy DirectAPI
directAPI = Proxy

prefixedAPI :: Proxy PrefixedAPI
prefixedAPI = Proxy

type ChatClient = Maybe Text -> Maybe Text -> Maybe Text -> ChatRequest -> ClientM ChatResponse

type GenerationClient = Maybe Text -> Maybe Text -> ClientM GenerationResponse

getClients :: Text -> (ChatClient, GenerationClient)
getClients provider =
  case provider of
    "openrouter.ai" ->
      let (chat :<|> gen) = client prefixedAPI
       in (chat, gen)
    _ ->
      -- Default to direct style
      let (chat :<|> gen) = client directAPI
       in (chat, gen)

tlsManagerSettingsOld :: ManagerSettings
tlsManagerSettingsOld =
  let settings = mkManagerSettings def def
   in settings {managerResponseTimeout = responseTimeoutMicro (600 * 1000000)}

tlsManagerSettings :: ManagerSettings
tlsManagerSettings =
  let settings = mkManagerSettings def def
      socketModifier socket = do
        -- Enable TCP keepalive at socket level
        setSocketOption socket KeepAlive 1

        -- On some platforms, you can set more specific keepalive parameters
        -- Uncomment if available on your platform:
        -- setSocketOption socket SocketOpts.TcpKeepIdle 60      -- Start after 60s idle
        -- setSocketOption socket SocketOpts.TcpKeepInterval 30   -- Send every 30s
        -- setSocketOption socket SocketOpts.TcpKeepCount 5       -- 5 retries

        -- Increase socket buffer sizes
        setSocketOption socket RecvBuffer 262144
        setSocketOption socket SendBuffer 262144
   in -- Disable Nagle's algorithm for faster sending of small packets
      -- setSocketOption socket SocketOpts.NoDelay 1
      settings
        { managerResponseTimeout = responseTimeoutMicro (600 * 1000000), -- 10 min timeout
          managerRawConnection = rawConnectionModifySocket socketModifier,
          -- Disable connection pooling for large requests
          managerConnCount = 1,
          managerIdleConnectionCount = 0
        }

sendQueryRaw :: Text -> Text -> Text -> Text -> Text -> Maybe Float -> [Message] -> IO (Either ClientError ChatResponse)
sendQueryRaw apiSite apiKey siteUrl siteName model temperature msgs = do
  manager <- newTlsManagerWith tlsManagerSettings
  let baseUrl = BaseUrl Https (T.unpack apiSite) 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  let auth = "Bearer " <> apiKey
  let request = ChatRequest model msgs Nothing temperature
  let (chatClient, _) = getClients apiSite
  runClientM (chatClient (Just auth) (Just siteUrl) (Just siteName) request) clientEnv

sendQueryStreaming :: Text -> Text -> Text -> Text -> Text -> Maybe Float -> [Message] -> IO (Either ClientError ChatResponse)
sendQueryStreaming apiSite apiKey siteUrl siteName model temperature msgs = do
  manager <- newTlsManagerWith tlsManagerSettings

  -- Build the raw URL, for example: https://<apiSite>/v1/chat/completions
  let url = "https://" <> T.unpack apiSite <> (if apiSite == "openrouter.ai" then "/api/v1/chat/completions" else "/v1/chat/completions")
  let baseUrl = BaseUrl Https (T.unpack apiSite) 443 ""
  initReq <- parseRequest url

  -- Construct the JSON payload. If you prefer, you can reuse ChatRequest & encode that:
  let payload =
        object
          [ "model" .= model,
            "messages" .= msgs,
            "stream" .= True,
            "temperature" .= temperature
          ]
  let requestBod = RequestBodyLBS (encode payload)
  let req =
        initReq
          { method = "POST",
            requestHeaders =
              [ ("Content-Type", "application/json"),
                ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),
                ("HTTP-Referer", TE.encodeUtf8 siteUrl),
                ("X-Title", TE.encodeUtf8 siteName)
              ],
            requestBody = requestBod,
            responseTimeout = responseTimeoutMicro (360 * 1000000)
          }
  let doIt x = withResponse x manager $ \resp -> do
        let status = responseStatus resp
        if statusCode status /= 200
          then do
            -- Non-OK status => convert to Servant-style FailureResponse
            bodyChunks <- brConsume resp.responseBody
            let servantReq =
                  SCC.Request
                    { requestPath = (baseUrl, ""),
                      requestQueryString = mempty,
                      requestBody = Just ((), "application/json"),
                      requestMethod = "POST",
                      requestAccept = mempty,
                      requestHeaders = mempty,
                      requestHttpVersion = resp.responseVersion
                    } ::
                    SCC.RequestF () (BaseUrl, ByteString)
            let servantResp =
                  Response
                    { responseStatusCode = status,
                      responseHeaders = Seq.fromList resp.responseHeaders,
                      responseHttpVersion = resp.responseVersion,
                      responseBody = BL.fromChunks bodyChunks
                    }
            pure (Left (FailureResponse servantReq servantResp))
          else do
            -- 200 OK => parse SSE chunks
            parseAllSSEChunks resp.responseBody
  withExceptionRetry (RetryConfig 3 1000000) doIt req

--------------------------------------------------------------------------------
-- SSE parsing (collect everything into a single ChatResponse)

newtype QueryId = MkQueryId Text
  deriving (Eq, Ord, Show)

parseAllSSEChunks ::
  BodyReader ->
  IO (Either ClientError ChatResponse)
parseAllSSEChunks bodyReader = do
  -- We'll keep track of:
  --   • leftover bytes that might span line boundaries
  --   • accumulated partial content (accContent)
  --   • possibly an id
  --   • possibly usage data
  --
  -- Once we see a "[DONE]" line or run out of chunks, we return.

  let loop ::
        ByteString ->
        -- \^ leftover partial line from previous chunk
        Text ->
        -- \^ accumulated text from all deltas
        Maybe QueryId ->
        -- \^ first 'id' we discover
        Maybe UsageData ->
        -- \^ last usage data we discover
        IO (Either ClientError ChatResponse)
      loop leftover accContent mId mUsage = do
        chunk <- brRead bodyReader
        if BS.null chunk
          -- No more data from server: finalize leftover as one “line” if not empty
          then
            if BS.null leftover
              then do
                -- No leftover, no chunk => we’re done
                putTextLn "Done as got null chunk and nothing leftover"
                pure (Right $ mkChatResponse accContent mUsage mId)
              else do
                -- Treat the leftover as one final line
                putTextLn $ "Done; handling ifnal leftover chunk " <> show leftover
                (finalContent, finalId, finalUsage, _isDone) <-
                  handleLine (accContent, mId, mUsage, False) leftover
                pure (Right $ mkChatResponse finalContent finalUsage finalId)
          else do
            let combined = leftover <> chunk
            -- Split on newline (10); each piece is a “line” except possibly the last
            let allPieces = BS.split 10 combined -- '\n' = 10
            let piecesCount = length allPieces

            -- Does this chunk end with newline?
            --   If so, the last piece after split is actually an empty line (all lines are “complete”).
            --   If not, the final piece is incomplete and becomes the new leftover.
            let chunkEndsWithNewline = (not (BS.null chunk)) && (BS.last chunk == 10)

            let (completeLines, newLeftover) =
                  if chunkEndsWithNewline
                    then
                      -- All pieces are complete lines;
                      -- often the last one is "" if chunk ended with a newline.
                      ( allPieces,
                        BS.empty
                      )
                    else case piecesCount of
                      0 -> ([], BS.empty) -- (shouldn't happen, but safe)
                      1 -> ([], L.head allPieces) -- everything is leftover
                      _ -> (L.init allPieces, L.last allPieces)

            -- Process each complete line in turn
            (updatedContent, updatedId, updatedUsage, done) <-
              foldM handleLine (accContent, mId, mUsage, False) completeLines

            if done
              then
                -- We saw [DONE], so we can return right away
                pure (Right $ mkChatResponse updatedContent updatedUsage updatedId)
              else
                -- Continue reading next chunk, with possible leftover
                loop newLeftover updatedContent updatedId updatedUsage

  -- Finally we start reading from the body with an empty leftover buffer
  loop BS.empty "" Nothing Nothing
  where
    handleLine ::
      (Text, Maybe QueryId, Maybe UsageData, Bool) ->
      ByteString ->
      IO (Text, Maybe QueryId, Maybe UsageData, Bool)
    handleLine (contentSoFar, idSoFar, usageSoFar, done) lineBS
      | done = pure (contentSoFar, idSoFar, usageSoFar, True) -- already found [DONE]
      | otherwise =
          case parseDataLine lineBS of
            Nothing -> do
              let line = T.strip $ TE.decodeUtf8 lineBS
              -- e.g., an SSE keep-alive or just empty “data:” lines that aren’t JSON
              when (BS.length lineBS > 0 && line /= ": OPENROUTER PROCESSING") $ putTextLn $ "Got unused line " <> show lineBS
              pure (contentSoFar, idSoFar, usageSoFar, False)
            Just "[DONE]" -> do
              -- Done signal
              putTextLn $ "STREAM_DONE. Content accumulated: " <> show (T.length contentSoFar)
              pure (contentSoFar, idSoFar, usageSoFar, True)
            Just jsonLine -> do
              -- Try to decode JSON
              case eitherDecodeStrict' (TE.encodeUtf8 (T.strip jsonLine)) of
                Left _err -> do
                  -- Not valid JSON => skip
                  putTextLn $ "Saw invalid json: " <> show _err
                  pure (contentSoFar, idSoFar, usageSoFar, False)
                Right val -> do
                  let (deltaText, queryId, newUsage) = extractStreamingData val
                      combinedContent = contentSoFar <> deltaText
                      usage' = usageSoFar <|> newUsage
                      queryId' = idSoFar <|> queryId
                  -- putTextLn $ "Got new val " <> show val <> " with new content " <> deltaText
                  pure (combinedContent, queryId', usage', False)

-- | SSE lines often look like "data: {...}" or "data: [DONE]".
parseDataLine :: ByteString -> Maybe Text
parseDataLine rawLine =
  let line = T.strip $ TE.decodeUtf8 rawLine
   in if "data: " `T.isPrefixOf` line
        then Just (T.drop 6 line) -- drop "data: "
        else Nothing

-- | Extract partial text from "choices[].delta.content", or maybe usage
extractStreamingData :: Value -> (Text, Maybe QueryId, Maybe UsageData)
extractStreamingData (Object o) =
  let mUsage =
        case HM.lookup "usage" o of
          Just usageVal -> decode (encode usageVal) -- decode to UsageData
          Nothing -> Nothing
      partialContent =
        case HM.lookup "choices" o of
          Just (Array arr) -> foldMap getDeltaText arr
          _ -> ""
      queryId =
        case HM.lookup "id" o of
          Just qId -> MkQueryId <$> decode (encode qId)
          _ -> Nothing
   in (partialContent, queryId, mUsage)
extractStreamingData _ = ("", Nothing, Nothing)

getDeltaText :: Value -> Text
getDeltaText (Object choiceObj) =
  case HM.lookup "delta" choiceObj of
    Just (Object deltaObj) ->
      case HM.lookup "content" deltaObj of
        Just (String c) -> c
        _ -> ""
    _ -> ""
getDeltaText _ = ""

-- | Build a final ChatResponse with a single Choice
mkChatResponse :: Text -> Maybe UsageData -> Maybe QueryId -> ChatResponse
mkChatResponse content mbUsage queryId =
  let c =
        Choice
          { message =
              Message
                { role = "assistant",
                  content = content
                }
          }
   in ChatResponse
        { id = case queryId of
            Just (MkQueryId x) -> x
            Nothing -> "",
          choices = [c],
          usage = case mbUsage of
            Just u -> u
            Nothing -> UsageData 0 0 0
        }

getGenerationStats :: Text -> Text -> Text -> IO (Either ClientError GenerationResponse)
getGenerationStats apiSite apiKey generationId = do
  manager <- newTlsManagerWith tlsManagerSettings
  let baseUrl = BaseUrl Https (T.unpack apiSite) 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  let auth = "Bearer " <> apiKey
  let (_, genClient) = getClients apiSite
  runClientM (genClient (Just generationId) (Just auth)) clientEnv

extractResult :: ChatResponse -> GenerationStats -> Either Text QueryResult
extractResult resp stats = case choices resp of
  (x : _) -> Right $ QueryResult x.message resp.usage stats
  [] -> Left "OpenRouter query returned no message!"

data ShouldLog = ShouldLog | ShouldNotLog
  deriving (Eq, Ord, Show)

retryWithDelay :: Int -> Int -> ShouldLog -> IO (Either ClientError a) -> IO (Either Text a)
retryWithDelay maxAttempts delayMicros shouldLog action = go maxAttempts
  where
    go 0 = pure $ Left "Max retry attempts exceeded"
    go n = do
      result <- action
      case result of
        Right x -> pure $ Right x
        Left err -> do
          when (shouldLog == ShouldLog) $ putTextLn $ "Error making query: " <> show err
          threadDelay delayMicros -- Wait before retrying
          go (n - 1)

-- Main query function with generation stats
sendQuery :: Text -> Text -> Text -> Text -> Text -> Maybe Float -> [Message] -> IO (Either Text QueryResult)
sendQuery apiSite apiKey siteUrl siteName model temperature msgs = do
  let initialQuery = if apiSite /= "openrouter.ai" && apiSite /= "api.deepseek.com" then sendQueryRaw else sendQueryStreaming
      numAttempts = 5
  Logging.logDebug "sendQuery" (T.unlines $ map renderMessage msgs)
  let queryWithEmptyCheck remainingAttempts = do
        Logging.logInfo "sendQueryAttempt" ("Attempt " <> show (numAttempts + 1 - remainingAttempts) <> " with " <> show (length msgs) <> " messages")
        result <- initialQuery apiSite apiKey siteUrl siteName model temperature msgs
        case result of
          Right resp ->
            -- Check if content is empty
            if T.null (content (L.head (choices resp)).message)
              then do
                Logging.logWarn "sendQuery" "Received empty response, retrying..."
                if remainingAttempts > 1
                  then threadDelay 5000000 >> queryWithEmptyCheck (remainingAttempts - 1)
                  else pure $ Right resp -- Give up and return empty response
              else pure $ Right resp
          Left err -> pure $ Left err
  queryResult <- retryWithDelay numAttempts 3000000 ShouldLog $ queryWithEmptyCheck numAttempts
  case queryResult of
    Left err -> pure $ Left $ "Error sending query to openrouter: " <> show err
    Right resp -> case apiSite == "openrouter.ai" of
      False -> pure $ extractResult resp $ GenerationStats 0 0 0
      True -> do
        statsResult <- retryWithDelay 5 3000000 ShouldNotLog $ getGenerationStats apiSite apiKey resp.id
        case statsResult of
          -- Left err -> pure $ Left $ "Error fetching generation stats: " <> show err
          Left err -> do
            putTextLn $ "Failed to get generation stats: " <> err
            pure $ extractResult resp $ GenerationStats 0 0 0
          Right statsResp -> pure $ extractResult resp (generationData statsResp)

-- Example usage:
example :: Text -> Text -> IO ()
example site apiKey = do
  let msg = Message "user" "What is the meaning of life?"
  result <-
    sendQuery
      site
      apiKey
      "https://yoursite.com"
      "YourSite"
      -- "o1-mini"
      -- "openai/o1-mini"
      -- "gpt-4o-mini"
      "openai/gpt-3.5-turbo"
      -- "deepseek-ai/DeepSeek-R1"
      -- "meta-llama/Llama-3.3-70B-Instruct"
      Nothing
      [msg]
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right response -> print response

-- Define a type for retry configuration
data RetryConfig = RetryConfig
  { maxRetries :: Int,
    delayMicroseconds :: Int -- Delay between retries
  }

-- Helper function to perform exponential backoff
withExponentialBackoff :: Int -> Int -> IO ()
withExponentialBackoff attempt delayMicros =
  threadDelay (delayMicros * (2 ^ attempt))

-- Main retry function
withExceptionRetry :: RetryConfig -> (a -> IO b) -> a -> IO b
withExceptionRetry config action req = go 0
  where
    go attempt =
      catches
        (action req)
        [ Handler (\(e :: HttpException) -> handleHttpException e attempt),
          Handler (\(e :: IOException) -> handleIOException e attempt)
        ]

    handleHttpException e attempt = do
      when (attempt > 0)
        $ withExponentialBackoff attempt (delayMicroseconds config)
      if attempt < maxRetries config
        then putTextLn ("Retrying after http exception " <> show e) >> go (attempt + 1)
        else error $ T.pack $ "Max retries reached: " ++ show e

    handleIOException e attempt = do
      when (attempt > 0)
        $ withExponentialBackoff attempt (delayMicroseconds config)
      if attempt < maxRetries config
        then putTextLn ("Retrying after io exception " <> show e) >> go (attempt + 1)
        else error $ T.pack $ "Max retries reached: " ++ show e
