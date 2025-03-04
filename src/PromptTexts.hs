{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module PromptTexts where

import Relude
import Text.RawString.QQ (r)

makeUnitTestPrompt :: Text -> Text -> Text -> Text -> Text
makeUnitTestPrompt fileName testFileName testName testSummary = "Please create a unit test for " <> fileName <> " in " <> testFileName <> " called " <> testName <> " that: " <> testSummary <> ". Note you're using doctest; tests will automatically run each time you modify the unit test file or the main source file. IF THE TEST FAILS BECAUSE OF A BUG IN THE SOURCE FILE BEING TESTED, YOU ARE RESPONSIBLE FOR FIXING THE SOURCE FILE; feel free to add more logging to the source files to assist in debugging test failures. For testing external APIs you may write tests that connect and wait e.g. 10 seconds to receive a response (and timeout if none is received), asserting that the response parses correctly and the result looks sane. Return true when you're satisfied with the test. Note that if any other tests (even in other files) are broken, you should fix those too before returning  (because you might have accidentally broken one somehow), opening whatever files are necessary. Remember to also open any helpful .txt/.md documentation from the list of available files. Remember the package is 'main', and local imports are project_name/file_name."

makeUnitTestsPrompt :: Text -> Text
makeUnitTestsPrompt fileName = makeUnitTestsPromptInner $ "Please decide upon some key unit tests to write for " <> fileName <> "."

makeUnitTestsForSpecificChangePrompt :: Text -> Text -> Text
makeUnitTestsForSpecificChangePrompt change fileName = makeUnitTestsPromptInner $ "Please decide upon some key unit tests to write for the change you just made: " <> change <> " in fileName " <> fileName <> "."

makeUnitTestsPromptInner :: Text -> Text
makeUnitTestsPromptInner starter = starter <> "Return a list of unit test names and descriptions in the format specified. If there are parts that will be hard to unit test fully (e.g. connecting to an external API), then you should design tests that just connect to the API and verify the downloaded/streamed data parses correctly and passes sanity assertions (with a timeout of e.g. 10 seconds that makes the test fail if no data is received). Remember to also open any helpful .txt/.md documentation from the list of available files."

makeSourcefilePrompt :: Text -> Text -> Text
makeSourcefilePrompt fileName fileSummary = "Please create the file " <> fileName <> " with contents: " <> fileSummary <> ". Try to write it in a testable manner, so unit tests can be written. Prefer a 'functional core, imperative shell' approach, where IO is done at an outer level and passed into pure functions (pure in the sense they do no IO, but they may mutate inputs in a deterministic way, as this is not Haskell). Remember to OpenFile other relevant files (which adds them to your context) when necessary. You can also EditFile existing project files where necessary. After creating the file with AppendFile (and editing with InsertInFile/EditFile as necessary until it compiles), which you'll see from the file being present and complete in OpenFiles, return the names and descriptions of the files created, in the format specified. Remember to also open any helpful .txt/.md documentation from the list of available files that may be useful as reference for the current task. Remember the package is 'main', and local imports are project_name/file_name. YOU MUST NOT RETURN UNTIL THE FULL FILE/IMPLEMENTATION IS COMPLETE, without any placeholders remaining!"

makeFilenamesPrompt :: Text
makeFilenamesPrompt =
  [r|
Please first plan a list of source files that the project will consist of, based on the intended architecture, along with outlines of their contents, and what other files they'll include as dependencies (only files to be created by the project, not including external dependencies, i.e. no need to mention boost etc.). The list of dependencies will be used in topologically sorting the files that need to be created such that each file is created before the files that depend on it (so no circular dependencies are allowed). Each file will have a unit test, so bear this in mind when arranging files; ideally each file should be like an independent module that depends on minimal files and can be tested in isolation. This is a single-step task so you don't need to write to the journal.txt (and cannot, because you don't have append/replace/edit tool permissions for this task). Don't mention unit tests files here; they'll be handled separately later. Please do however do include as a dependency any blahDocSummary.txt someDoc.md you see in availableFiles that might be useful to a particular file.

NOTE: Please put all files in the root directory of the project, not subdirectories, for simplicity. And all in package main.
|]

makeRefactorBackgroundPrompt :: Text -> Text
makeRefactorBackgroundPrompt taskDescription =
  [r|
Your current project is refactoring. First you'll, for each source file, analyse it for any changes needed. Then you'll combine these lists of changes for all files, and go through implementing them file by file, adding unit tests for each change. You'll also create any other files you deem necessary.
|]
    <> taskDescription

makeArchitectureDesignPrompt :: Text
makeArchitectureDesignPrompt = "Please think carefully and design the architecture for the project given the requirements above, returning a detailed description of the planned architecture."

binanceSummary :: Text
binanceSummary =
  [r|
Note the binance websocket API descriptions describe how to build a book from depth streams and snapshots based on their sequence ID. Building a book isn't required, but the saved data should be such that it can be used to build a book. If a market data book-building message is missed, then a new snapshot should be requested from the rest API as described in the docs. The relevant docs for it are in binanceApiDetails.txt, you should OpenFile=<[{ "fileName": "binanceApiDetails.txt" }]> (or "binanceApiDetails_CoinMFutures.txt" for futures) to view them when needed. You should also store regular snapshots from the rest API, e.g. once every minute.

Binance websocket API docs are available in the folder for reference; note that spot and futures have slightly different formats. The application should download trade and aggregate trade streams (the formats are different), and book data (everything necessary for building/tracking an orderbook; there are both diff and update streams, _it should subscribe to both_), and best price data, but doesn't need the historical summary messages. For futures it should also store the mark price data.

There should be a single normalised trade format that should be a superset of information from the different kind of trade messages. It should also have a numTrades field, capturing the number of trades (just one, for a single event), and windowSizeMs (how big the time window, for aggregated trades published e.g. every 100ms it'd be 100). All kinds of trades should be converted into this format then saved. In this format, there is a separate record for each price level traded, but if the original format doesn't specify exact per-price traded quantities then instead the min_price and max_price fields are used (in the normal case, min_price and max_price as the same price).

Aside from the normalised trade format, there should also be a normalised level-current-qty format (which both snapshots and the book update messages are converted into), that should also contain a snapshot/message ID incremented after every message (so individual updates can be identified wrt whether they belong in the same message or not). There should also be a normalised level-qty-change message, separate from the level-current-qty, used for the update messages (which show qty changes instead of the new qty). Both the above message types should record an update for each price level (i.e. we don't store any record with a variable number of price/qty fields, instead we store a separate record for each price, but with a messageID field of some sort so those belong to the same original update can be identified). The level-qty-change fields should also store all the underlying ids necessary for building a book from snapshots, and should re-request a snapshot any time an update is missed.

There should also be a normalised bbo update message, containing a superset of the information in both exchanges' BBO update messages, and a normalised mark price message (although only futures have a mark price).

In total, the above gives 5 normalised message types to record into Parquet: trade, level_qty_change, level_new_qty, bbo_update, and mark_price_update. Each should include all timestamps provided by the exchange (which may be optional/zero in some cases), as well as the time the recording process received the message.

The program should take as config a JSON file, with a list of exchange/feedcode pairs. There should be separate objects with the same interface for each exchange (only two exchanges for now, binance_spot and binance_cfutures but more will be added later), and some function that handles dispatching a subscription to the right exchange based on the name. There should also be a single subscriber interface that each separate exchange implements, but they should share the basic websocket code, which should include reconnection logic for when it disconnects, as the exchange sometimes does.

The main file should be minimal, and just pass the config and hook up/start the multiExchangeSubscriber and recorder. The individual exchange subscribers should have tests that each kind of subscription they make successfully receives and parses data (as well as the snapshots they request).

For unit testing the binance API websocket connection/rest API components, unit tests should be written that just connect to a busy product (i.e. BTCUSDT), wait at most e.g. 10 seconds to recieve data, then try parsing it and assert that it matches the expected format, failing and printing how it doesn't match (along with the whole text/json) if it doesn't, to make it easy for you to see what's wrong and fix the code.

Note the Binance market data APIs are public and don't need an API key.

|]

approachSummary :: Text
approachSummary =
  [r|
It's designed to be an LLM-developed and managed application, so the design should involve many small files, allowing the LLM to easily read and update them without needing to keep large files in context. The amount of dependencies among files should be minimised, so that each file can be understood with a minimum number of other files in context (but at the same time, putting all dependencies in a single source file isn't ideal if that file is really big the aim is to minimise context needed, not just number of files).

IMPORTANT NOTE: To keep context size minimal you won't see your full previous responses/history, so I recommend keeping a journal in "journal.txt", which you can append a short summary of what you're trying to do at each step, and what you plan to do next, in case you expect to have future steps in that task and don't want to lose your train of thought between responses.

|]

    -- When you make changes to a source file, if compilation and unit tests pass then the file is added and committed to a git repo in the base directory. You may use the Revert tool where necessary to get back to the last version of a file when compilation and all tests passed.

projectSummary :: Text -> Text
projectSummary projectName = projectSummaryGolang projectName <> approachSummary <> binanceSummary

projectSummaryGolang :: Text -> Text
projectSummaryGolang projectName =
  [r|
You are an agent involved in coding a Go application. It's a binance market data recorder, but the code will later be used in HFT so minimising dynamic allocation and cache misses is important. You should record the data to Parquet files. It should support subscribing to multiple instruments, but should use a separate stream for each. The only external library you need to use is github.com/xitongsys/parquet-go. Documentation is available in the directory for your reference. Each type of market data should be stored in a separate file. There should be one file per exchange/instrument/date (UTC data), and the filename should contain the exchange (with binance futures and binance being separate exchanges), instrument and date. It should support resuming recording to an existing file after restart.

NOTE: the imports for parquet-go are:
"github.com/xitongsys/parquet-go/parquet"
"github.com/xitongsys/parquet-go/source"
"github.com/xitongsys/parquet-go/reader"
"github.com/xitongsys/parquet-go/writer"
"github.com/xitongsys/parquet-go-source/local"
DO NOT attempt to import it any other ways as it will fail (you may remember the old repo structure, which has now changed), and DO NOT attempt to modify "mod.go". Please open the documentation when working with the parquet-go library.

Please use the gorilla websockets lib.
|]
    <> "NOTE: the project name is "
    <> projectName
    <> ", which you'll need for importing subdirectories,e.g. include \""
    <> projectName
    <> "/someSubDir\"."

projectSummaryCPlusPlus :: Text
projectSummaryCPlusPlus =
  [r|
You are an agent involved in coding a C++ application. It's a binance market data recorder, but the code will later be used in HFT so minimising dynamic allocation and cache misses is important. simdjson is used for parsing json, spdlog for logging, and boost::describe for deserialisation from JSON (via simdjson), formatting, and generating strings for inserting into ClickHouse (and creating tables where they don't exist, as well as generating query strings for unit tests). The whole application is single-threaded, with a polling-based architecture. libwebsockets is used for the websocket connection, and cpp-httplib for requesting the book snapshots.

You're using GCC 12.2.0, which supports most of C++20 and some C++23.

Unit tests should use doctest, and for every C++ filename.h/cc, there should be a corresponding filename_test.cc . The compiler will run (and show you the output if compilation fails) whenever a source file is changed, and run any tests too and show you if they fail. Note if you write a .h file, it won't get compiled until it's included in a .cc, so you can add it to main.cc to get a compilation result immediately.

The application should take all required config as JSON, deserialised automatically with boost::describe.

Note that for the different types, no manual JSON or ClickHouse (de)serialisation code should be written, instead generic to/from json and to/from ClickHouse functions that utilise boost::describe to work with any POD structs should be written, and of course those functions will need unit tests.

You can assume a ClickHouse instance with no password running on localhost.

The working way to include the external dependencies is:
#include "clickhouse/client.h"
#include <libwebsockets.h>
#include "spdlog/spdlog.h"
#include "httplib.h"

For the unit tests, they should:
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

|]

binanceApiDoc :: Text
binanceApiDoc =
  [r|
# Web Socket Streams for Binance (2024-12-17)

## General WebSocket Information
- Base endpoints: 
  - wss://stream.binance.com:9443
  - wss://stream.binance.com:443
- Streams can be accessed as single raw streams or combined streams
- Raw streams format: /ws/<streamName>
- Combined streams format: /stream?streams=<streamName1>/<streamName2>/<streamName3>
- Combined stream events are wrapped as: {"stream":"<streamName>","data":<rawPayload>}
- All symbols must be lowercase
- Connections are valid for 24 hours only
- Server sends ping frame every 3 minutes - must respond with pong frame within 10 minutes
- Market data only streams available at wss://data-stream.binance.vision
- For microsecond timestamps, add timeUnit=MICROSECOND to URL

## WebSocket Limits
- 5 incoming messages per second limit (pings, pongs, and control messages)
- Maximum 1024 streams per connection
- 300 connections per 5 minutes per IP

## Stream Management
You can subscribe/unsubscribe to streams dynamically. The ID field can be:
- 64-bit signed integer
- Alphanumeric string (max 36 chars)
- null

### Example Subscribe Request:
```json
{
  "method": "SUBSCRIBE",
  "params": [
    "btcusdt@aggTrade",
    "btcusdt@depth"
  ],
  "id": 1
}
```

### Example Unsubscribe Request:
```json
{
  "method": "UNSUBSCRIBE", 
  "params": [
    "btcusdt@depth"
  ],
  "id": 312
}
```

### Available Streams Include:

1. Aggregate Trade Streams
2. Trade Streams
3. Kline/Candlestick Streams (UTC and timezone offset versions)
4. Individual Symbol Mini Ticker
5. All Market Mini Tickers
6. Individual Symbol Ticker
7. All Market Tickers
8. Individual Symbol Book Ticker
9. Average Price
10. Partial Book Depth
11. Diff Depth Stream

Each stream type has specific payload formats and update frequencies.

## Managing Local Order Books
Detailed steps are provided for maintaining a local order book:
1. Open WebSocket connection
2. Buffer events
3. Get depth snapshot
4. Validate sequence numbers
5. Apply updates in correct order
6. Maintain update IDs

Note: Depth snapshots are limited to 5000 price levels per side.

The documentation includes complete payload examples and error handling guidance for each stream type.

I'll provide the detailed specifications for those specific streams:

## 1. Trade Streams
Shows individual trades as they occur.

**Stream Name:** `<symbol>@trade`  
**Update Speed:** Real-time

**Example Payload:**
```json
{
  "e": "trade",         // Event type
  "E": 1672515782136,   // Event time
  "s": "BNBBTC",        // Symbol
  "t": 12345,           // Trade ID
  "p": "0.001",         // Price
  "q": "100",           // Quantity
  "T": 1672515782136,   // Trade time
  "m": true,            // Is buyer market maker?
  "M": true             // Ignore
}
```

## 2. Aggregate Trade Streams
Shows trades that have been aggregated by price level.

**Stream Name:** `<symbol>@aggTrade`  
**Update Speed:** Real-time

**Example Payload:**
```json
{
  "e": "aggTrade",      // Event type
  "E": 1672515782136,   // Event time
  "s": "BNBBTC",        // Symbol
  "a": 12345,           // Aggregate trade ID
  "p": "0.001",         // Price
  "q": "100",           // Quantity
  "f": 100,             // First trade ID
  "l": 105,             // Last trade ID
  "T": 1672515782136,   // Trade time
  "m": true,            // Is buyer market maker?
  "M": true             // Ignore
}
```

## 3. Individual Symbol Book Ticker
Streams best bid/ask prices and quantities for a symbol.

**Stream Name:** `<symbol>@bookTicker`  
**Update Speed:** Real-time

**Example Payload:**
```json
{
  "u": 400900217,     // Order book updateId
  "s": "BNBUSDT",     // Symbol
  "b": "25.35190000", // Best bid price
  "B": "31.21000000", // Best bid qty
  "a": "25.36520000", // Best ask price
  "A": "40.66000000"  // Best ask qty
}
```

## 4. Partial Book Depth Streams
Shows top bids and asks at specified depth.

**Stream Name:** `<symbol>@depth<levels>` or `<symbol>@depth<levels>@100ms`  
**Update Speed:** 1000ms or 100ms  
**Levels:** 5, 10, or 20

**Example Payload:**
```json
{
  "lastUpdateId": 160,
  "bids": [           // Bids to be updated
    [
      "0.0024",       // Price level
      "10"            // Quantity
    ]
  ],
  "asks": [           // Asks to be updated
    [
      "0.0026",       // Price level
      "100"           // Quantity
    ]
  ]
}
```

## 5. Diff. Depth Stream
Provides depth updates for order book maintenance.

**Stream Name:** `<symbol>@depth` or `<symbol>@depth@100ms`  
**Update Speed:** 1000ms or 100ms

**Example Payload:**
```json
{
  "e": "depthUpdate",     // Event type
  "E": 1672515782136,     // Event time
  "s": "BNBBTC",          // Symbol
  "U": 157,               // First update ID
  "u": 160,               // Final update ID
  "b": [                  // Bids to be updated
    [
      "0.0024",           // Price level
      "10"                // Quantity
    ]
  ],
  "a": [                  // Asks to be updated
    [
      "0.0026",           // Price level
      "100"               // Quantity
    ]
  ]
}
```

**Important Note for Depth Streams:**
For maintaining an accurate local order book:
1. First event's U should be compared with snapshot's lastUpdateId
2. Ignore any event where u is <= lastUpdateId of snapshot
3. Update local book by:
   - Adding new price levels
   - Updating quantities for existing levels
   - Removing levels when quantity is 0
4. Always validate update IDs to ensure sequence integrity

The depth snapshots have a limit of 5000 price levels per side. Price levels beyond this limit won't be visible unless they change.

Order Book Snapshot REST API

To reliably maintain an order book, Binance provides REST endpoints to fetch the initial book snapshot:

    Spot: GET https://api.binance.com/api/v3/depth?symbol=<SYMBOL>&limit=<N>
    USDT-M Futures: GET https://fapi.binance.com/fapi/v1/depth?symbol=<SYMBOL>&limit=<N>
    Coin-M Futures: GET https://dapi.binance.com/dapi/v1/depth?symbol=<SYMBOL>&limit=<N>

These endpoints return the current order book up to N levels (N can be 5, 10, 20, 50, 100, 500, 1000, or 5000 depending on the market).

The response JSON looks like:

{
  "lastUpdateId": 160,
  "bids": [
    ["0.0024", "10"],    // price, quantity
    ... up to N bids
  ],
  "asks": [
    ["0.0026", "100"],
    ... up to N asks
  ]
}

For example, for spot BNBBTC with limit=5000, you’d get up to 5000 bids and 5000 asks, and a lastUpdateId​. The lastUpdateId is crucial: it represents the state of the order book at that snapshot. Any diff stream updates earlier than or up to this ID are already reflected in the snapshot and should be discarded.

Maintaining a Local Order Book (Combining WS + REST)

Binance’s documentation outlines a specific sequence to properly maintain an order book locally with WS updates. The general algorithm is:

    Connect to the Depth WebSocket stream. For example, for spot: wss://stream.binance.com:9443/ws/BNBBTC@depth (or @depth@100ms for faster updates). For futures: wss://fstream.binance.com/stream?streams=btcusdt@depth (this could also be a combined stream URL). Begin receiving depth update events. Buffer these events (do not apply them yet).
    Get an initial depth snapshot via REST. For spot, GET .../api/v3/depth?symbol=BNBBTC&limit=5000; for futures, .../fapi/v1/depth?symbol=BTCUSDT&limit=1000. This returns lastUpdateId and lists of bids/asks.
    Discard outdated WS events. Let lastUpdateId = the snapshot’s last update ID. Drop any buffered websocket event where the event’s u (final update ID) <= lastUpdateId. Those events are already included in the snapshot.
    Ensure sequence continuity. The first websocket event after the snapshot should have U (first update ID in that event) <= lastUpdateId+1 and u >= lastUpdateId+1. In other words, the snapshot’s ID should fall between U and u of the event. If not, it means you missed events and need to get a new snapshot and start over.
    Apply remaining buffered events to the snapshot. Update your local order book using the bids and asks from each event (in order). For each price in the event:
        If the quantity (q) is 0: remove that price level from your book (if present)​
        developers.binance.com
        .
        If q is > 0: update/add that price level to the new quantity (this is an absolute quantity, not a delta)​
        developers.binance.com
        .
    Continue to process new events in real-time. For each new event from the stream, apply the updates as in step 5. Also, verify the continuity: each event’s pu (previous update ID) should equal the last event’s u that you processed. If you ever find a gap (i.e., event.pu != last_seen_update_id), you should resync by going back to step 2 (get a new snapshot).
    Book ticker stream (optional): If only the best bid/ask is needed, you can use the <symbol>@bookTicker stream which directly gives you the current best prices without maintaining the full book. But for full order depth, you must use the diff stream + snapshot as above.

|]

binanceFuturesApiDoc :: Text
binanceFuturesApiDoc =
  [r|
The connection method for Websocket is：

    Base Url: wss://fstream.binance.com
    Streams can be access either in a single raw stream or a combined stream
    Raw streams are accessed at /ws/<streamName>
    Combined streams are accessed at /stream?streams=<streamName1>/<streamName2>/<streamName3>
    Example:
    wss://fstream.binance.com/ws/bnbusdt@aggTrade
    wss://fstream.binance.com/stream?streams=bnbusdt@aggTrade/btcusdt@markPrice

Combined stream events are wrapped as follows: {"stream":"<streamName>","data":<rawPayload>}

All symbols for streams are lowercase

A single connection is only valid for 24 hours; expect to be disconnected at the 24 hour mark

The websocket server will send a ping frame every 3 minutes. If the websocket server does not receive a pong frame back from the connection within a 10 minute period, the connection will be disconnected. Unsolicited pong frames are allowed(the client can send pong frames at a frequency higher than every 15 minutes to maintain the connection).

WebSocket connections have a limit of 10 incoming messages per second.

A connection that goes beyond the limit will be disconnected; IPs that are repeatedly disconnected may be banned.

A single connection can listen to a maximum of 200 streams.

Considering the possible data latency from RESTful endpoints during an extremely volatile market, it is highly recommended to get the order status, position, etc from the Websocket user data stream.

Live Subscribing/Unsubscribing to streams

    The following data can be sent through the websocket instance in order to subscribe/unsubscribe from streams. Examples can be seen below.
    The id used in the JSON payloads is an unsigned INT used as an identifier to uniquely identify the messages going back and forth.

Subscribe to a stream

    Response

{
  "result": null,
  "id": 1
}

    Request

    {
    "method": "SUBSCRIBE",
    "params":
    [
    "btcusdt@aggTrade",
    "btcusdt@depth"
    ],
    "id": 1
    }

Unsubscribe to a stream

    Response

{
  "result": null,
  "id": 312
}

    Request

    {
    "method": "UNSUBSCRIBE",
    "params":
    [
    "btcusdt@depth"
    ],
    "id": 312
    }


Aggregate Trade Streams
Stream Description

The Aggregate Trade Streams push market trade information that is aggregated for fills with same price and taking side every 100 milliseconds. Only market trades will be aggregated, which means the insurance fund trades and ADL trades won't be aggregated.
Stream Name

<symbol>@aggTrade
Update Speed

100ms
Response Example

{
  "e": "aggTrade",  // Event type
  "E": 123456789,   // Event time
  "s": "BTCUSDT",    // Symbol
  "a": 5933014,		// Aggregate trade ID
  "p": "0.001",     // Price
  "q": "100",       // Quantity
  "f": 100,         // First trade ID
  "l": 105,         // Last trade ID
  "T": 123456785,   // Trade time
  "m": true,        // Is the buyer the market maker?
}

Partial Book Depth Streams
Stream Description

Top <levels> bids and asks, Valid <levels> are 5, 10, or 20.
Stream Name

<symbol>@depth<levels> OR <symbol>@depth<levels>@500ms OR <symbol>@depth<levels>@100ms.
Update Speed

250ms, 500ms or 100ms
Response Example

{
  "e": "depthUpdate", // Event type
  "E": 1571889248277, // Event time
  "T": 1571889248276, // Transaction time
  "s": "BTCUSDT",
  "U": 390497796,     // First update ID in event
  "u": 390497878,     // Final update ID in event
  "pu": 390497794,    // Final update Id in last stream(ie `u` in last stream)
  "b": [              // Bids to be updated
    [
      "7403.89",      // Price Level to be updated
      "0.002"         // Quantity
    ],
    [
      "7403.90",
      "3.906"
    ],
    [
      "7404.00",
      "1.428"
    ],
    [
      "7404.85",
      "5.239"
    ],
    [
      "7405.43",
      "2.562"
    ]
  ],
  "a": [              // Asks to be updated
    [
      "7405.96",      // Price level to be
      "3.340"         // Quantity
    ],
    [
      "7406.63",
      "4.525"
    ],
    [
      "7407.08",
      "2.475"
    ],
    [
      "7407.15",
      "4.800"
    ],
    [
      "7407.20",
      "0.175"
    ]
  ]
}

Diff. Book Depth Streams
Stream Description

Bids and asks, pushed every 250 milliseconds, 500 milliseconds, 100 milliseconds (if existing)
Stream Name

<symbol>@depth OR <symbol>@depth@500ms OR <symbol>@depth@100ms
Update Speed

250ms, 500ms, 100ms
Response Example

{
  "e": "depthUpdate", // Event type
  "E": 123456789,     // Event time
  "T": 123456788,     // Transaction time 
  "s": "BTCUSDT",     // Symbol
  "U": 157,           // First update ID in event
  "u": 160,           // Final update ID in event
  "pu": 149,          // Final update Id in last stream(ie `u` in last stream)
  "b": [              // Bids to be updated
    [
      "0.0024",       // Price level to be updated
      "10"            // Quantity
    ]
  ],
  "a": [              // Asks to be updated
    [
      "0.0026",       // Price level to be updated
      "100"          // Quantity
    ]
  ]
}

How to manage a local order book correctly

    Open a stream to wss://fstream.binance.com/stream?streams=btcusdt@depth.
    Buffer the events you receive from the stream. For same price, latest received update covers the previous one.
    Get a depth snapshot from https://fapi.binance.com/fapi/v1/depth?symbol=BTCUSDT&limit=1000 .
    Drop any event where u is < lastUpdateId in the snapshot.
    The first processed event should have U <= lastUpdateId**AND**u >= lastUpdateId
    While listening to the stream, each new event's pu should be equal to the previous event's u, otherwise initialize the process from step 3.
    The data in each event is the absolute quantity for a price level.
    If the quantity is 0, remove the price level.
    Receiving an event that removes a price level that is not in your local order book can happen and is normal.

Individual Symbol Book Ticker Streams
Stream Description

Pushes any update to the best bid or ask's price or quantity in real-time for a specified symbol.
Stream Name

<symbol>@bookTicker
Update Speed

Real-time
Response Example

{
  "e":"bookTicker",			// event type
  "u":400900217,     		// order book updateId
  "E": 1568014460893,  		// event time
  "T": 1568014460891,  		// transaction time
  "s":"BNBUSDT",     		// symbol
  "b":"25.35190000", 		// best bid price
  "B":"31.21000000", 		// best bid qty
  "a":"25.36520000", 		// best ask price
  "A":"40.66000000"  		// best ask qty
}

Mark Price Stream
Stream Description

Mark price and funding rate for a single symbol pushed every 3 seconds or every second.
Stream Name

<symbol>@markPrice or <symbol>@markPrice@1s
Update Speed

3000ms or 1000ms
Response Example

  {
    "e": "markPriceUpdate",  	// Event type
    "E": 1562305380000,      	// Event time
    "s": "BTCUSDT",          	// Symbol
    "p": "11794.15000000",   	// Mark price
    "i": "11784.62659091",		// Index price
    "P": "11784.25641265",		// Estimated Settle Price, only useful in the last hour before the settlement starts
    "r": "0.00038167",       	// Funding rate
    "T": 1562306400000       	// Next funding time
  }



Order Book (HTTP, not websocket)
API Description

Query symbol orderbook
HTTP Request

GET /fapi/v1/depth
Request Weight

Adjusted based on the limit:
Limit	Weight
5, 10, 20, 50	2
100	5
500	10
1000	20
Request Parameters
Name	Type	Mandatory	Description
symbol	STRING	YES	
limit	INT	NO	Default 500; Valid limits:[5, 10, 20, 50, 100, 500, 1000]
Response Example

{
  "lastUpdateId": 1027024,
  "E": 1589436922972,   // Message output time
  "T": 1589436922959,   // Transaction time
  "bids": [
    [
      "4.00000000",     // PRICE
      "431.00000000"    // QTY
    ]
  ],
  "asks": [
    [
      "4.00000200",
      "12.00000000"
    ]
  ]
}

|]
