{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BinanceApiDataRecorder where

import AppConfig
import Control.Monad.Except
import Core
import Data.Text qualified as T
import GoLang (GoLang)
import Logging qualified
import PromptCommon
import PromptTexts qualified
import Relude
import System.FilePath qualified as FP
import System.Log.Logger qualified as Logger
import Text.RawString.QQ (r)

makeGoBinanceApiDataRecorder :: AppConfig -> ModelConfig -> IO ()
makeGoBinanceApiDataRecorder aCfg mCfg = do
  let cfg = appAndModelConfigToConfig aCfg mCfg
      dependencies =
        [ "github.com/xitongsys/parquet-go",
          "github.com/xitongsys/parquet-go-source",
          "github.com/apache/thrift",
          "github.com/gorilla/websocket",
          "github.com/xitongsys/parquet-go/parquet",
          "github.com/xitongsys/parquet-go/reader",
          "github.com/xitongsys/parquet-go/writer"
        ]
      initialFiles =
        [ ("binanceApiDetails.txt", binanceApiDoc),
          ("binanceApiDetails_CoinMFutures.txt", binanceFuturesApiDoc),
          ("parquet-go-readme.md", parquetDoc)
        ]
      projectCfg =
        ProjectConfig
          { projectDependencyNames = dependencies,
            projectInitialFiles = initialFiles
          }
      backgroundTexts = projectSummary . T.pack $ configBaseDir cfg
      projectTexts = ProjectTexts {projectSummaryText = backgroundTexts}
      initialState = AppState mempty [] [] (CompileTestState Nothing Nothing 0)
      logDir = T.unpack $ logFileDir aCfg
      logPath = logDir FP.</> "promptyped_binapi_downloader.log"
      debugLogPath = logDir FP.</> "promptyped_binapi_downloader.debug.log"
  Logging.initializeLogger logPath debugLogPath Logger.INFO
  liftIO $ Logging.logInfo "Initial config" (show cfg)
  liftIO $ Logging.logInfo "Initial state" (show initialState)
  let projectFn :: AppM ()
      projectFn = case (projectKind aCfg) of
        CreateProject -> makeCreateFilesProject @GoLang projectTexts projectCfg
        RefactorProject -> case bigRefactorCfg aCfg of
          Just refactorCfg -> makeRefactorFilesProject @GoLang projectTexts refactorCfg
          Nothing -> throwError "Missing big refactor config!"
        FileAnalysisProject -> makeFileAnalysisProject @GoLang projectTexts
        TargetedRefactorProject -> case targetedRefactorCfg aCfg of
          Just refactorCfg -> makeTargetedRefactorProject @GoLang projectTexts refactorCfg
          Nothing -> throwError "Missing targeted refactor config!"
  res <- runApp cfg initialState projectFn
  case res of
    Left err -> putTextLn $ "Process ended with error: " <> show err
    Right ((), finalState) -> do
      liftIO $ Logging.logInfo "Final config" (show cfg)
      liftIO $ Logging.logInfo "Final state" (show $ stateMetrics finalState)
      liftIO . putTextLn . show $ cfg
      liftIO . putTextLn . show $ stateMetrics finalState

sampleBigRefactorCfg :: BigRefactorConfig
sampleBigRefactorCfg =
  BigRefactorConfig
    { bigRefactorInitialOpenFiles = ["binanceApiDetails_CoinMFutures.txt"],
      bigRefactorOverallTask =
        "YOUR OBJECTIVE is to refactor the project to add support for Binance CoinM futures market data (it currently only supports Binance spot), as described in binanceApiDetails_CoinMFutures.txt."
          <> "Note that the datatypes may be slightly different than for Binance spot; when this is the case you should create different structs for each, and store them in different parquet tables to the existing types."
          <> "The data should be saved to filenames containing the kind (spot or future), date and instrument, not just the date and instrument."
          <> "The config should be kind,instrument pairs, not just instrument, and depending on kind the code will properly pick and connect to Binance Spot or Futures."
          <> "You need to support aggregate trade streams, individual symbol book ticker streams, partial book depth streams, diff book depth streams, and mark price streams. Remember to implement logic so the data can be used for managing a local orderbook correctly, as already done for Binance Spot; how to do this is described in the doc.",
      bigRefactorOverallTaskShortName = "Add support for Binance CoinM futures"
    }

projectSummary :: Text -> Text
projectSummary projectName = projectSummaryGolang projectName <> PromptTexts.approachSummary <> binanceSummary

projectSummaryGolang :: Text -> Text
projectSummaryGolang projectName =
  [r|
You are an agent involved in coding a Go application. It's a binance market data recorder, but the code will later be used in HFT so minimising dynamic allocation and cache misses is important. You should record the data to Parquet files. It should support subscribing to multiple instruments, but should use a separate stream for each. The only external library you need to use is github.com/xitongsys/parquet-go. Documentation is available in the directory for your reference. Each type of market data should be stored in a separate file. There should be one file per exchange/instrument/date (UTC data), and the filename should contain the exchange (with binance futures and binance being separate exchanges), instrument and date. It should not support resuming recording to an existing file after restart, as Parquet doesn't support this.

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

parquetDoc :: Text
parquetDoc =
  [r|
# parquet-go

[![Travis Status for xitongsys/parquet-go](https://app.travis-ci.com/xitongsys/parquet-go.svg?branch=master)](https://app.travis-ci.com/github/xitongsys/parquet-go)
[![godoc for xitongsys/parquet-go](https://godoc.org/github.com/nathany/looper?status.svg)](http://godoc.org/github.com/xitongsys/parquet-go)

parquet-go is a pure-go implementation of reading and writing the parquet format file.

* Support Read/Write Nested/Flat Parquet File
* Simple to use
* High performance

## Install

Add the parquet-go library to your $GOPATH/src and install dependencies:

```sh
go get github.com/xitongsys/parquet-go
```

## Examples

The `example/` directory contains several examples.

The `local_flat.go` example creates some data and writes it out to the `example/output/flat.parquet` file.

```sh
cd $GOPATH/src/github.com/xitongsys/parquet-go/example
go run local_flat.go
```

The `local_flat.go` code shows how it's easy to output `structs` from Go programs to Parquet files.

## Type

There are two types in Parquet: Primitive Type and Logical Type. Logical types are stored as primitive types.

### Primitive Type
|Primitive Type|Go Type|
|-|-|
|BOOLEAN|bool|
|INT32|int32|
|INT64|int64|
|INT96([deprecated](https://github.com/xitongsys/parquet-go/issues/420))|string|
|FLOAT|float32|
|DOUBLE|float64|
|BYTE_ARRAY|string|
|FIXED_LEN_BYTE_ARRAY|string|


### Logical Type
|Logical Type|Primitive Type|Go Type|
|-|-|-|
|UTF8|BYTE_ARRAY|string|
|INT_8|INT32|int32|
|INT_16|INT32|int32|
|INT_32|INT32|int32|
|INT_64|INT64|int64|
|UINT_8|INT32|int32|
|UINT_16|INT32|int32|
|UINT_32|INT32|int32|
|UINT_64|INT64|int64|
|DATE|INT32|int32|
|TIME_MILLIS|INT32|int32|
|TIME_MICROS|INT64|int64|
|TIMESTAMP_MILLIS|INT64|int64|
|TIMESTAMP_MICROS|INT64|int64|
|INTERVAL|FIXED_LEN_BYTE_ARRAY|string|
|DECIMAL|INT32,INT64,FIXED_LEN_BYTE_ARRAY,BYTE_ARRAY|int32,int64,string,string|
|LIST|-|slice||
|MAP|-|map||

### Tips
* Parquet-go supports type alias such `type MyString string`. But the base type must follow the table instructions.

* Some type convert functions: [converter.go](https://github.com/xitongsys/parquet-go/blob/master/types/converter.go)

## Encoding

#### PLAIN:

All types

#### PLAIN_DICTIONARY/RLE_DICTIONARY:

All types

#### DELTA_BINARY_PACKED:

INT32, INT64, INT_8, INT_16, INT_32, INT_64, UINT_8, UINT_16, UINT_32, UINT_64, TIME_MILLIS, TIME_MICROS, TIMESTAMP_MILLIS, TIMESTAMP_MICROS

#### DELTA_BYTE_ARRAY:

BYTE_ARRAY, UTF8

#### DELTA_LENGTH_BYTE_ARRAY:

BYTE_ARRAY, UTF8

### Tips

* Some platforms don't support all kinds of encodings. If you are not sure, just use PLAIN and PLAIN_DICTIONARY.
* If the fields have many different values, please don't use PLAIN_DICTIONARY encoding. Because it will record all the different values in a map which will use a lot of memory. Actually it use a 32-bit integer to store the index. It can not used if your unique values number is larger than 32-bit.
* Large array values may be duplicated as min and max values in page stats, significantly increasing file size. If stats are not useful for such a field, they can be omitted from written files by adding `omitstats=true` to a field tag.

## Repetition Type

There are three repetition types in Parquet: REQUIRED, OPTIONAL, REPEATED.

|Repetition Type|Example|Description|
|-|-|-|
|REQUIRED|```V1 int32 `parquet:"name=v1, type=INT32"` ```|No extra description|
|OPTIONAL|```V1 *int32 `parquet:"name=v1, type=INT32"` ```|Declare as pointer|
|REPEATED|```V1 []int32 `parquet:"name=v1, type=INT32, repetitiontype=REPEATED"` ```|Add 'repetitiontype=REPEATED' in tags|

### Tips

* The difference between a List and a REPEATED variable is the 'repetitiontype' in tags. Although both of them are stored as slice in go, they are different in parquet. You can find the detail of List in parquet at [here](https://github.com/apache/parquet-format/blob/master/LogicalTypes.md). I suggest just use a List.
* For LIST and MAP, some existed parquet files use some nonstandard formats(see [here](https://github.com/apache/parquet-format/blob/master/LogicalTypes.md)). For standard format, parquet-go will convert them to go slice and go map. For nonstandard formats, parquet-go will convert them to corresponding structs.

## Example of Type and Encoding

```golang
Bool              bool    `parquet:"name=bool, type=BOOLEAN"`
Int32             int32   `parquet:"name=int32, type=INT32"`
Int64             int64   `parquet:"name=int64, type=INT64"`
Int96             string  `parquet:"name=int96, type=INT96"`
Float             float32 `parquet:"name=float, type=FLOAT"`
Double            float64 `parquet:"name=double, type=DOUBLE"`
ByteArray         string  `parquet:"name=bytearray, type=BYTE_ARRAY"`
FixedLenByteArray string  `parquet:"name=FixedLenByteArray, type=FIXED_LEN_BYTE_ARRAY, length=10"`

Utf8             string `parquet:"name=utf8, type=BYTE_ARRAY, convertedtype=UTF8, encoding=PLAIN_DICTIONARY"`
Int_8            int32   `parquet:"name=int_8, type=INT32, convertedtype=INT32, convertedtype=INT_8"`
Int_16           int32  `parquet:"name=int_16, type=INT32, convertedtype=INT_16"`
Int_32           int32  `parquet:"name=int_32, type=INT32, convertedtype=INT_32"`
Int_64           int64  `parquet:"name=int_64, type=INT64, convertedtype=INT_64"`
Uint_8           int32  `parquet:"name=uint_8, type=INT32, convertedtype=UINT_8"`
Uint_16          int32 `parquet:"name=uint_16, type=INT32, convertedtype=UINT_16"`
Uint_32          int32 `parquet:"name=uint_32, type=INT32, convertedtype=UINT_32"`
Uint_64          int64 `parquet:"name=uint_64, type=INT64, convertedtype=UINT_64"`
Date             int32  `parquet:"name=date, type=INT32, convertedtype=DATE"`
Date2            int32  `parquet:"name=date2, type=INT32, convertedtype=DATE, logicaltype=DATE"`
TimeMillis       int32  `parquet:"name=timemillis, type=INT32, convertedtype=TIME_MILLIS"`
TimeMillis2      int32  `parquet:"name=timemillis2, type=INT32, logicaltype=TIME, logicaltype.isadjustedtoutc=true, logicaltype.unit=MILLIS"`
TimeMicros       int64  `parquet:"name=timemicros, type=INT64, convertedtype=TIME_MICROS"`
TimeMicros2      int64  `parquet:"name=timemicros2, type=INT64, logicaltype=TIME, logicaltype.isadjustedtoutc=false, logicaltype.unit=MICROS"`
TimestampMillis  int64  `parquet:"name=timestampmillis, type=INT64, convertedtype=TIMESTAMP_MILLIS"`
TimestampMillis2 int64  `parquet:"name=timestampmillis2, type=INT64, logicaltype=TIMESTAMP, logicaltype.isadjustedtoutc=true, logicaltype.unit=MILLIS"`
TimestampMicros  int64  `parquet:"name=timestampmicros, type=INT64, convertedtype=TIMESTAMP_MICROS"`
TimestampMicros2 int64  `parquet:"name=timestampmicros2, type=INT64, logicaltype=TIMESTAMP, logicaltype.isadjustedtoutc=false, logicaltype.unit=MICROS"`
Interval         string `parquet:"name=interval, type=BYTE_ARRAY, convertedtype=INTERVAL"`

Decimal1 int32  `parquet:"name=decimal1, type=INT32, convertedtype=DECIMAL, scale=2, precision=9"`
Decimal2 int64  `parquet:"name=decimal2, type=INT64, convertedtype=DECIMAL, scale=2, precision=18"`
Decimal3 string `parquet:"name=decimal3, type=FIXED_LEN_BYTE_ARRAY, convertedtype=DECIMAL, scale=2, precision=10, length=12"`
Decimal4 string `parquet:"name=decimal4, type=BYTE_ARRAY, convertedtype=DECIMAL, scale=2, precision=20"`

Decimal5 int32 `parquet:"name=decimal5, type=INT32, logicaltype=DECIMAL, logicaltype.precision=10, logicaltype.scale=2"`

Map      map[string]int32 `parquet:"name=map, type=MAP, convertedtype=MAP, keytype=BYTE_ARRAY, keyconvertedtype=UTF8, valuetype=INT32"`
List     []string         `parquet:"name=list, type=MAP, convertedtype=LIST, valuetype=BYTE_ARRAY, valueconvertedtype=UTF8"`
Repeated []int32          `parquet:"name=repeated, type=INT32, repetitiontype=REPEATED"`
```

## Compression Type

|Type|Support|
|-|-|
| CompressionCodec_UNCOMPRESSED | YES|
|CompressionCodec_SNAPPY|YES|
|CompressionCodec_GZIP|YES|
|CompressionCodec_LZO|NO|
|CompressionCodec_BROTLI|NO|
|CompressionCodec_LZ4 |YES|
|CompressionCodec_ZSTD|YES|

## ParquetFile

Read/Write a parquet file need a ParquetFile interface implemented

```golang
type ParquetFile interface {
io.Seeker
io.Reader
io.Writer
io.Closer
Open(name string) (ParquetFile, error)
Create(name string) (ParquetFile, error)
}
```

Using this interface, parquet-go can read/write parquet file on different platforms. All the file sources are at [parquet-go-source](https://github.com/xitongsys/parquet-go-source). Now it supports(local/hdfs/s3/gcs/memory).

## Writer

Four Writers are supported: ParquetWriter, JSONWriter, CSVWriter, ArrowWriter.

* ParquetWriter is used to write predefined Golang structs.
[Example of ParquetWriter](https://github.com/xitongsys/parquet-go/blob/master/example/local_flat.go)

* JSONWriter is used to write JSON strings
[Example of JSONWriter](https://github.com/xitongsys/parquet-go/blob/master/example/json_write.go)

* CSVWriter is used to write data format similar with CSV(not nested)
[Example of CSVWriter](https://github.com/xitongsys/parquet-go/blob/master/example/csv_write.go)

* ArrowWriter is used to write parquet files using Arrow Schemas
[Example of ArrowWriter](https://github.com/xitongsys/parquet-go/blob/master/example/arrow_to_parquet.go)

## Reader

Two Readers are supported: ParquetReader, ColumnReader

* ParquetReader is used to read predefined Golang structs
[Example of ParquetReader](https://github.com/xitongsys/parquet-go/blob/master/example/local_nested.go)

* ColumnReader is used to read raw column data. The read function return 3 slices([value], [RepetitionLevel], [DefinitionLevel]) of the records.
[Example of ColumnReader](https://github.com/xitongsys/parquet-go/blob/master/example/column_read.go)

### Tips

* If the parquet file is very big (even the size of parquet file is small, the uncompressed size may be very large), please don't read all rows at one time, which may induce the OOM. You can read a small portion of the data at a time like a stream-oriented file.

* `RowGroupSize` and `PageSize` may influence the final parquet file size. You can find the details from [here](https://github.com/apache/parquet-format). You can reset them in ParquetWriter
```go
pw.RowGroupSize = 128 * 1024 * 1024 // default 128M
pw.PageSize = 8 * 1024 // default 8K
```

## Schema

There are four methods to define the schema: go struct tags, Json, CSV, Arrow metadata. Only items in schema will be written and others will be ignored.

### Tag

```golang
type Student struct {
Name    string  `parquet:"name=name, type=BYTE_ARRAY, convertedtype=UTF8, encoding=PLAIN_DICTIONARY"`
Age     int32   `parquet:"name=age, type=INT32, encoding=PLAIN"`
Id      int64   `parquet:"name=id, type=INT64"`
Weight  float32 `parquet:"name=weight, type=FLOAT"`
Sex     bool    `parquet:"name=sex, type=BOOLEAN"`
Day     int32   `parquet:"name=day, type=INT32, convertedtype=DATE"`
Ignored int32   //without parquet tag and won't write
}
```

[Example of tags](https://github.com/xitongsys/parquet-go/blob/master/example/local_flat.go)

### JSON

JSON schema can be used to define some complicated schema, which can't be defined by tag.

```golang
type Student struct {
NameIn    string
Age     int32
Id      int6 and others will be ignored.

### Tag

```golang
type Student struct {
Name    string  `patring

Friends []struct {
Name string
Id   int64
}
Teachers []struct {
Name string
Id   int64
}
}

var jsonSchema string = `
{
  "Tag": "name=parquet_go_root, repetitiontype=REQUIRED",
    "Fields": [
        {"Tag": "name=name, inname=NameIn, type=BYTE_ARRAY, convertedtype=UTF8, repetitiontype=REQUIRED"},
            {"Tag": "name=age, inname=Age, type=INT32, repetitiontype=REQUIRED"},
                {"Tag": "name=id, inname=Id, type=INT64, repetitiontype=REQUIRED"},
                    {"Tag": "name=weight, inname=Weight, type=FLOAT, repetitiontype=REQUIRED"},
                        {"Tag": "name=sex, inname=Sex, type=BOOLEAN, repetitiontype=REQUIRED"},

    {"Tag": "name=classes, inname=Classes, type=LIST, repetitiontype=REQUIRED",
         "Fields": [{"Tag": "name=element, type=BYTE_ARRAY, convertedtype=UTF8, repetitiontype=REQUIRED"}]
             },

    {
          "Tag": "name=scores, inname=Scores, type=MAP, repetitiontype=REQUIRED",
                "Fields": [
                        {"Tag": "name=key, type=BYTE_ARRAY, convertedtype=UTF8, repetitiontype=REQUIRED"},
                                {"Tag": "name=value, type=LIST, repetitiontype=REQUIRED",
                                         "Fields": [{"Tag": "name=element, type=FLOAT, repetitiontype=REQUIRED"}]
                                                 }
                                                       ]
                                                           },

    {
          "Tag": "name=friends, inname=Friends, type=LIST, repetitiontype=REQUIRED",
                "Fields": [
                       {"Tag": "name=element, repetitiontype=REQUIRED",
                               "Fields": [
                                        {"Tag": "name=name, inname=Name, type=BYTE_ARRAY, convertedtype=UTF8, repetitiontype=REQUIRED"},
                                                 {"Tag": "name=id, inname=Id, type=INT64, repetitiontype=REQUIRED"}
                                                         ]}
                                                               ]
                                                                   },

    {
          "Tag": "name=teachers, inname=Teachers, repetitiontype=REPEATED",
                "Fields": [
                        {"Tag": "name=name, inname=Name, type=BYTE_ARRAY, convertedtype=UTF8, repetitiontype=REQUIRED"},
                                {"Tag": "name=id, inname=Id, type=INT64, repetitiontype=REQUIRED"}
                                      ]
                                          }
                                            ]
                                            }
                                            `
                                            ```
                                            [Example of JSON schema](https://github.com/xitongsys/parquet-go/blob/master/example/json_schema.go)


### CSV metadata

```golang
md := []string{
"name=Name, type=BYTE_ARRAY, convertedtype=UTF8, encoding=PLAIN_DICTIONARY",
"name=Age, type=INT32",
"name=Id, type=INT64",
"name=Weight, type=FLOAT",
"name=Sex, type=BOOLEAN",
}
```

[Example of CSV metadata](https://github.com/xitongsys/parquet-go/blob/master/example/csv_write.go)

### Arrow metadata

```golang
schema := arrow.NewSchema(
[]arrow.Field{
{Name: "int64", Type: arrow.PrimitiveTypes.Int64},
{Name: "float64", Type: arrow.PrimitiveTypes.Float64},
{Name: "str", Type: arrow.BinaryTypes.String},
},
nil,
)
```

[Example of Arrow metadata](https://github.com/xitongsys/parquet-go/blob/master/example/arrow_to_parquet.go)

### Tips

* Parquet-go reads data as an object in Golang and every field must be a public field, which start with an upper letter. This field name we call it `InName`. Field name in parquet file we call it `ExName`. Function `common.HeadToUpper` converts `ExName` to `InName`. There are some restriction:
1. It's not allowed if two field names are only different at their first letter case. Such as `name` and `Name`.
2. `PARGO_PREFIX_` is a reserved string, which you'd better not use it as a name prefix. ([#294](https://github.com/xitongsys/parquet-go/issues/294))
3. Use `\x01` as the delimiter of fields to support `.` in some field name.([dot_in_name.go](https://github.com/xitongsys/parquet-go/blob/master/example/dot_in_name.go), [#349](https://github.com/xitongsys/parquet-go/issues/349))

## Concurrency

Marshal/Unmarshal is the most time consuming process in writing/reading. To improve the performance, parquet-go can use multiple goroutines to marshal/unmarshal the objects. You can set the concurrent number parameter `np` in the Read/Write initial functions.

```golang
func NewParquetReader(pFile ParquetFile.ParquetFile, obj interface{}, np int64) (*ParquetReader, error)
func NewParquetWriter(pFile ParquetFile.ParquetFile, obj interface{}, np int64) (*ParquetWriter, error)
func NewJSONWriter(jsonSchema string, pfile ParquetFile.ParquetFile, np int64) (*JSONWriter, error)
func NewCSVWriter(md []string, pfile ParquetFile.ParquetFile, np int64) (*CSVWriter, error)
func NewArrowWriter(arrowSchema *arrow.Schema, pfile source.ParquetFile, np int64) (*ArrowWriter error)
```

## Examples

|Example file|Descriptions|
|-|-|
|[local_flat.go](https://github.com/xitongsys/parquet-go/blob/master/example/local_flat.go)|write/read parquet file with no nested struct|
|[local_nested.go](https://github.com/xitongsys/parquet-go/blob/master/example/local_nested.go)|write/read parquet file with nested struct|
|[read_partial.go](https://github.com/xitongsys/parquet-go/blob/master/example/read_partial.go)|read partial fields from a parquet file|
|[read_partial2.go](https://github.com/xitongsys/parquet-go/blob/master/example/read_partial2.go)|read sub-struct from a parquet file|
|[read_without_schema_predefined.go](https://github.com/xitongsys/parquet-go/blob/master/example/read_without_schema_predefined.go)|read a parquet file and no struct/schema predefined needed|
|[read_partial_without_schema_predefined.go](https://github.com/xitongsys/parquet-go/blob/master/example/read_partial_without_schema_predefined.go)|read sub-struct from a parquet file and no struct/schema predefined needed|
|[json_schema.go](https://github.com/xitongsys/parquet-go/blob/master/example/json_schema.go)|define schema using json string|
|[json_write.go](https://github.com/xitongsys/parquet-go/blob/master/example/json_write.go)|convert json to parquet|
|[convert_to_json.go](https://github.com/xitongsys/parquet-go/blob/master/example/convert_to_json.go)|convert parquet to json|
|[csv_write.go](https://github.com/xitongsys/parquet-go/blob/master/example/csv_write.go)|special csv writer|
|[column_read.go](https://github.com/xitongsys/parquet-go/blob/master/exampler/example/column_read.go)|read raw column data and return value,repetitionLevel,definitionLevel|
|[type.go](https://github.com/xitongsys/parquet-go/blob/master/example/type.go)|example for schema of types|
|[type_alias.go](https://github.com/xitongsys/parquet-go/blob/master/example/type_alias.go)|example for type alias|
|[writer.go](https://github.com/xitongsys/parquet-go/blob/master/example/writer.go)|create ParquetWriter from io.Writer|
|[keyvalue_metadata.go](https://github.com/xitongsys/parquet-go/blob/master/example/keyvalue_metadata.go)|write keyvalue metadata|
|[dot_in_name.go](https://github.com/xitongsys/parquet-go/blob/master/example/dot_in_name.go)|`.` in filed name|
|[arrow_to_parquet.go](https://github.com/xitongsys/parquet-go/blob/master/example/arrow_to_parquet.go)|write/read parquet file using arrow definition|


## Tool

* [parquet-tools](https://github.com/xitongsys/parquet-go/blob/master/tool/parquet-tools): Command line tools that aid in the inspection of Parquet files

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
