{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CPlusPlus where

import BuildSystem (BuildSystem (..))
import Control.Exception (IOException, try)
import Core
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import FileSystem (runProcessWithTimeout)
import PromptTexts (binanceApiDoc)
import Relude
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.FilePath qualified as FP
import Text.RawString.QQ (r)

setupDirectoryCpp :: FilePath -> IO (Maybe Text)
setupDirectoryCpp dir = do
  result <- try $ do
    -- Create build directory
    Dir.createDirectoryIfMissing True (dir FP.</> "build")

    -- Write CMakeLists.txt
    TIO.writeFile (dir FP.</> "CMakeLists.txt") cmakeContents

    -- Write main.cc
    TIO.writeFile (dir FP.</> "main.cc") mainContents

    TIO.writeFile (dir FP.</> "sample_test.cc") sampleTestFile

    TIO.writeFile (dir FP.</> "lib.cc") ""

    TIO.writeFile (dir FP.</> "journal.txt") ""

    let docs =
          [ ("clickhouseDocSummary.txt", clickhouseDoc),
            ("boostDescribeDocSummary.txt", boostDescribeDoc),
            ("simdJsonDocSummary.txt", simdJsonDoc),
            ("spdlogDocSummary.txt", spdlogDoc),
            ("libwebsocketsDocSummary.txt", libwebsocketsDoc),
            ("cppHttplibDocSummary.txt", cppHttplibDoc),
            ("doctestDocSummary.txt", doctestDoc),
            ("binanceApiDetails.txt", binanceApiDoc)
          ]
    forM_ docs (\(name, doc) -> TIO.writeFile (dir FP.</> name) doc)

  case result of
    Left err -> pure $ Just $ T.pack $ displayException (err :: IOException)
    Right () -> pure Nothing
  where
    mainContents =
      T.unlines
        [ "#include <iostream>",
          "#include \"clickhouse/client.h\"",
          "#include <libwebsockets.h>",
          "#include \"spdlog/spdlog.h\"",
          "#include \"httplib.h\"",
          "",
          "int main() {",
          "    std::cout << \"Hello, World!\" << std::endl;",
          "    return 0;",
          "}"
        ]

    cmakeContents = mainCmakeFile

-- | Build the project with detailed error reporting
buildProjectCpp ::
  -- | Number of make jobs
  Int ->
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  IO (Maybe Text)
buildProjectCpp numJobs timeout dir newEnv = Dir.withCurrentDirectory (dir FP.</> "build") $ do
  -- Run cmake
  putTextLn $ "Building in dir " <> T.pack dir
  cmakeResult <- runProcessWithTimeout timeout "." newEnv "cmake" [".."]
  case cmakeResult of
    Left err -> pure $ Just err
    Right (exitCode, stdoutRes, stderrRes) -> case exitCode of
      Exit.ExitSuccess -> do
        -- Run make
        let makeArgs = ["-j" <> show numJobs]
        makeResult <- runProcessWithTimeout timeout "." newEnv "make" makeArgs
        case makeResult of
          Left err -> pure $ Just err
          Right (makeExit, makeOut, makeErr) -> case makeExit of
            Exit.ExitSuccess -> pure Nothing
            Exit.ExitFailure code ->
              pure
                $ Just
                $ "Make failed with exit code: "
                <> T.pack (show code)
                <> "\n"
                <> "stdout:\n"
                <> makeOut
                <> "\n"
                <> "stderr:\n"
                <> makeErr
      Exit.ExitFailure code ->
        pure
          $ Just
          $ "CMake failed with exit code: "
          <> T.pack (show code)
          <> "\n"
          <> "stdout:\n"
          <> stdoutRes
          <> "\n"
          <> "stderr:\n"
          <> stderrRes

-- | Run tests with output capture and timeout
runTestsCpp ::
  -- | Timeout in seconds
  NominalDiffTime ->
  -- | Project directory
  FilePath ->
  -- | Environment variables
  [(String, String)] ->
  IO (Maybe Text)
runTestsCpp timeout dir newEnv = Dir.withCurrentDirectory (dir FP.</> "build") $ do
  putTextLn $ "Testing in dir " <> T.pack dir
  result <- runProcessWithTimeout timeout "." newEnv "ctest" ["--output-on-failure"]
  case result of
    Left err -> pure $ Just err
    Right (exitCode, stdoutRes, stderrRes) -> case exitCode of
      Exit.ExitSuccess -> pure Nothing
      Exit.ExitFailure code ->
        pure
          $ Just
          $ "Tests failed with exit code: "
          <> T.pack (show code)
          <> "\n"
          <> "stdout:\n"
          <> stdoutRes
          <> "\n"
          <> "stderr:\n"
          <> stderrRes

isCPlusPlusFileExtension :: Text -> Bool
isCPlusPlusFileExtension fileName = ".h" `T.isSuffixOf` fileName || ".cc" `T.isSuffixOf` fileName || ".cpp" `T.isSuffixOf` fileName || ".hpp" `T.isSuffixOf` fileName

data CPlusPlusLang = CPlusPlusLang

instance BuildSystem CPlusPlusLang where
  buildProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    let numJobs = configBuildNumJobs cfg
    liftIO $ buildProjectCpp numJobs timeout baseDir envVars

  testProject cfg = do
    envVars <- getEnvVars
    let baseDir = configBaseDir cfg
    let timeout = secondsToNominalDiffTime . fromIntegral $ configBuildTimeoutSeconds cfg
    liftIO $ runTestsCpp timeout baseDir envVars

  setupProject cfg = liftIO $ setupDirectoryCpp $ configBaseDir cfg

  isBuildableFile fileName = pure $ isCPlusPlusFileExtension fileName

  getIgnoredDirs = pure ["build", ".git", "contrib"]

  getFormatChecker _ = do
    let alwaysPass = pure Nothing :: IO (Maybe Text)
    pure alwaysPass

sampleTestFile :: Text
sampleTestFile =
  [r|
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

int factorial(int number) { return number <= 1 ? number : factorial(number - 1) * number; }

TEST_CASE("testing the factorial function") {
  CHECK(factorial(1) == 1);
  CHECK(factorial(2) == 2);
  CHECK(factorial(3) == 6);
  CHECK(factorial(10) == 3628800);
}

|]

mainCmakeFile :: Text
mainCmakeFile =
  [r|
cmake_minimum_required(VERSION 3.15)
project(MDBench LANGUAGES CXX)
include(ExternalProject)

# Enable testing (optional)
enable_testing()

# Set C++ standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include FetchContent
include(FetchContent)

###############################################################################
# 1) Find or fetch dependencies
###############################################################################

#
# OpenSSL first (required by libwebsockets)
#
find_package(OpenSSL REQUIRED)

find_package(libwebsockets REQUIRED)

#
# libwebsockets (requires OpenSSL)
#
# FetchContent_Declare(
#     libwebsockets
#     GIT_REPOSITORY https://github.com/warmcat/libwebsockets.git
#     GIT_TAG v4.3.3
#     CMAKE_ARGS
#         -DLWS_WITH_SSL=ON
#         -DLWS_WITHOUT_TESTAPPS=ON
#         -DLWS_WITHOUT_TEST_SERVER=ON
#         -DLWS_WITHOUT_TEST_SERVER_EXTPOLL=ON	
#         -DLWS_WITHOUT_TEST_=ON
#         -DLWS_WITHOUT_TEST_CLIENT=ON
#         -DLWS_WITH_SHARED=OFF  # Build static library by default
#         -DLWS_SSL_CLIENT_USE_OS_CA_CERTS=ON  # Use system CA certificates
#         # If needed, specify:
#         # -DOPENSSL_ROOT_DIR=${OPENSSL_ROOT_DIR}
# )
# FetchContent_MakeAvailable(libwebsockets)

#
# simdjson
#

# ExternalProject_Add(fetch-simdjson
#   GIT_REPOSITORY https://github.com/simdjson/simdjson.git
#   GIT_TAG v3.12.0
#   SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/simdjson
#   CONFIGURE_COMMAND ""
#   BUILD_COMMAND ""
#   INSTALL_COMMAND ""
# )
# include_directories(${CMAKE_CURRENT_SOURCE_DIR}/contrib/simdjson/singleheader)

FetchContent_Declare(
  simdjson
  GIT_REPOSITORY https://github.com/simdjson/simdjson.git
  GIT_TAG v3.12.0
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/simdjson
)
FetchContent_MakeAvailable(simdjson)

#
# spdlog
#
ExternalProject_Add(fetch-spdlog
  GIT_REPOSITORY https://github.com/gabime/spdlog.git
  GIT_TAG v1.15.1
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/spdlog
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/contrib/spdlog/include)

#
# cpp-httplib
#
#FetchContent_Declare(
#    cpp-httplib
#    GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
#    GIT_TAG v0.15.3
#)
#FetchContent_MakeAvailable(cpp-httplib)
ExternalProject_Add(fetch-cpp-httplib
  GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
  GIT_TAG v0.18.5
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/cpp-httplib
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/contrib/cpp-httplib)

#
# doctest
#
#FetchContent_Declare(
#    doctest
#    GIT_REPOSITORY https://github.com/doctest/doctest.git
#    GIT_TAG v2.4.11
#)
#FetchContent_MakeAvailable(doctest)

ExternalProject_Add(fetch-doctest
  GIT_REPOSITORY https://github.com/doctest/doctest.git
  GIT_TAG v2.4.11
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/doctest
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
  )
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/contrib/doctest/doctest)

# clickhouse-cpp
#

# ExternalProject_Add(fetch-clickhouse-cpp
#   GIT_REPOSITORY https://github.com/ClickHouse/clickhouse-cpp
#   GIT_TAG v2.5.1
#   SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/contrib/clickhouse-cpp
#   CONFIGURE_COMMAND ""
#   BUILD_COMMAND ""
#   INSTALL_COMMAND ""
# )
#add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/contrib/clickhouse-cpp)

FetchContent_Declare(
  clickhouse-cpp
  GIT_REPOSITORY https://github.com/ClickHouse/clickhouse-cpp.git
  GIT_TAG v2.5.1
  CMAKE_ARGS
    -DCLICKHOUSE_CPP_BUILD_TESTS=OFF
    -DCLICKHOUSE_CPP_BUILD_EXAMPLES=OFF
)
FetchContent_MakeAvailable(clickhouse-cpp)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/contrib/clickhouse-cpp)

#
# Boost (header-only use: "describe" is in Boost::boost)
#
set(BOOST_MIN_VERSION "1.81.0")
find_package(Boost ${BOOST_MIN_VERSION} REQUIRED)

###############################################################################
# 2) Gather sources
###############################################################################

file(GLOB ALL_SOURCES
    "*.cpp"
    "*.cc"
)

# All test files
file(GLOB TEST_SOURCES "*_test.cc")

# The main file(s)
file(GLOB MAIN_SOURCE "main.cc")

# Filter out main and test sources from the "library" sources
set(LIB_SOURCES ${ALL_SOURCES})
list(REMOVE_ITEM LIB_SOURCES ${MAIN_SOURCE})
foreach(TEST_SRC ${TEST_SOURCES})
    list(REMOVE_ITEM LIB_SOURCES ${TEST_SRC})
endforeach()

###############################################################################
# 3) Build the internal library
###############################################################################

add_library(${PROJECT_NAME}_lib
    ${LIB_SOURCES}
)

FetchContent_GetProperties(clickhouse-cpp)
message(STATUS "clickhouse-cpp source dir: ${clickhouse-cpp_SOURCE_DIR}")

target_include_directories(${PROJECT_NAME}_lib PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${simdjson_SOURCE_DIR}/include
  ${clickhouse-cpp_SOURCE_DIR}
  ${clickhouse-cpp_SOURCE_DIR}/contrib/absl
)

# Link dependencies. For header-only Boost Describe, just use Boost::boost.
target_link_libraries(${PROJECT_NAME}_lib PRIVATE
    clickhouse-cpp-lib
    websockets                # or libwebsockets::websockets, check actual name
    simdjson
    Boost::boost
    OpenSSL::SSL
    OpenSSL::Crypto
)

###############################################################################
# 4) Main executable
###############################################################################

add_executable(${PROJECT_NAME}
    ${MAIN_SOURCE}
)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_NAME}_lib)

###############################################################################
# 5) Tests
###############################################################################

foreach(TEST_SRC ${TEST_SOURCES})
    get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_SRC})
    target_link_libraries(${TEST_NAME} PRIVATE
        ${PROJECT_NAME}_lib
    )
    add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
endforeach()

|]

clickhouseDoc :: Text
clickhouseDoc =
  [r|
ClickHouse C++ API

Using the ClickHouse C++ client library (clickhouse-cpp) allows you to connect to a ClickHouse database and execute SQL queries from C++ code. Below are common tasks like creating tables, inserting data, and querying data using this API:
Creating a Table

Initialize a clickhouse::Client with connection options (e.g. host) and use Client::Execute to run a CREATE TABLE SQL statement. For example, to create an in-memory table with two columns (id and name):

Client client(ClientOptions().SetHost("localhost"));

// Create a table with UInt64 and String columns.
client.Execute("CREATE TABLE IF NOT EXISTS default.numbers (id UInt64, name String) ENGINE = Memory");

In the above snippet, a table named default.numbers is created with the specified schema.
Inserting Data

To insert data, prepare a clickhouse::Block and append columns of data to it. Each column is created as a ColumnUInt64, ColumnString, etc., corresponding to the table schema. Then call Client::Insert with the table name and the block:

Block block;
auto id_col   = std::make_shared<ColumnUInt64>();
auto name_col = std::make_shared<ColumnString>();

// Append values to columns
id_col->Append(1); 
id_col->Append(7);
name_col->Append("one");
name_col->Append("seven");

// Add columns to the block with their names
block.AppendColumn("id", id_col);
block.AppendColumn("name", name_col);

// Insert the block into the table
client.Insert("default.numbers", block);

This code builds a block of two rows (id: 1 and 7, name: "one" and "seven") and inserts it into the default.numbers table. The library handles sending the data to ClickHouse in columnar format.
Querying Data

To query data, use Client::Select with a SELECT SQL query and provide a callback to process each block of the result:

client.Select("SELECT id, name FROM default.numbers", [] (const Block& block) {
    for (size_t i = 0; i < block.GetRowCount(); ++i) {
        UInt64 id   = block[0]->As<ColumnUInt64>()->At(i);
        std::string name = block[1]->As<ColumnString>()->At(i);
        std::cout << id << " " << name << "\n";
    }
});

This example selects all id and name values from the table and prints them. Within the callback, each Column in the Block can be accessed and cast to the appropriate column type to retrieve values.

Note: After finishing, you can drop the table if desired: client.Execute("DROP TABLE default.numbers");. Also, keep in mind that a Client instance is not thread-safe; use separate clients per thread if needed.

|]

boostDescribeDoc :: Text
boostDescribeDoc =
  [r|
Boost::Describe

Boost.Describe is a C++14 reflection library that allows you to obtain information about the members of structs/classes (and enums) at compile time. It’s useful for writing generic code that needs to iterate over struct members – for example, to automate generation of SQL statements or JSON serialization for any struct.
Describing Structs and Classes

To use Boost.Describe, you must annotate your types with macros provided by the library:

    BOOST_DESCRIBE_STRUCT(<Type>, <Bases>, <Members>) – Non-intrusively describe a struct’s public data members (and/or member functions). It’s placed outside the struct definition (in the same namespace)​
    boost.org
    ​
    boost.org
    . You list the type, any base classes, and the member names.
    BOOST_DESCRIBE_CLASS(<Type>, <Bases>, <Public Members>, <Protected Members>, <Private Members>) – Intrusively describe a class (placed inside the class definition). This can include non-public members by providing lists for each access level​
    boost.org
    ​
    boost.org
    .

For example, to describe a simple struct:

struct X {
    int m1;
    std::string m2;
};
BOOST_DESCRIBE_STRUCT(X, (), (m1, m2))

This makes X’s members available to the Boost.Describe reflection utilities. Describing a type does not require modifying the struct’s definition (for public members), which means you can even describe third-party or standard library types by writing the macro in your own code
.
Accessing Described Members

Boost.Describe provides templates to retrieve lists of member descriptors:

    boost::describe::describe_members<T, Modifiers> yields a type list of member descriptors for type T​
    boost.org
    . Modifiers allow filtering (e.g., mod_public, mod_private, mod_function, etc.).
    Each member descriptor in the list has a static name (the member name as a string) and pointer (a pointer-to-member to access that member)​
    boost.org
    .

Using Boost.MP11 (a metaprogramming utility), you can iterate over members at compile-time. For instance, you could define a generic operator<< for structs to print all members:

#include <boost/describe.hpp>
#include <boost/mp11.hpp>
using namespace boost::describe;

template<class T,
         class Md = describe_members<T, mod_any_access>,
         class En = std::enable_if_t<!std::is_union<T>::value>>
std::ostream& operator<<(std::ostream& os, const T& t) {
    os << "{";
    bool first = true;
    // Iterate over described members
    boost::mp11::mp_for_each<Md>([&](auto D){
        if (!first) os << ", ";
        first = false;
        os << D.name << " = " << t.*D.pointer;  // print "memberName = value"
    });
    os << "}";
    return os;
}

This uses describe_members<T, mod_any_access> to get all members of T (public, inherited, etc.) and then mp_for_each to loop through them. For each member descriptor D, D.name is the member’s name and t.*D.pointer accesses the member’s value on the struct t. This way, the code works for any described struct T – enabling generic serialization functions such as converting a struct to a SQL INSERT statement or constructing a CREATE TABLE schema string.
JSON Serialization with Boost.Describe

Starting with Boost 1.81, Boost.JSON can integrate with Boost.Describe to automatically serialize/deserialize described types to JSON. If a struct has been annotated with BOOST_DESCRIBE_STRUCT, you can leverage boost::json::value_from and boost::json::value_to (with the proper boost::json::object conversions) without writing boilerplate, thanks to an internal use of Describe.

There are also third-party libraries like DAW JSON Link that use Boost.Describe. For example, daw_json_link_describe allows conversion of described structs to/from JSON by simply adding a specialization and using daw::json::from_json<T> / to_json:

struct X { int m1; int m2; };
BOOST_DESCRIBE_STRUCT(X, (), (m1, m2))

std::string_view json_data = R"({ "m1": 55, "m2": 123 })";
X obj = daw::json::from_json<X>(json_data);
assert(obj.m1 == 55 && obj.m2 == 123);
std::string out = daw::json::to_json(obj);

In the above code, after describing X, the DAW JSON library is able to parse JSON into an X and serialize X back to JSON with minimal code. This demonstrates the power of Boost.Describe for generic serialization tasks.
|]

simdJsonDoc :: Text
simdJsonDoc =
  [r|

simdjson is a high-performance JSON parsing library in C++ that can parse gigabytes of JSON per second by using SIMD CPU instructions. It provides two main APIs:

    DOM API: parses the entire JSON into a DOM (document object model) that you can navigate (similar to RapidJSON or nlohmann::json).
    On-Demand API: allows iterating through JSON data in a forward-only manner without building an in-memory tree, which is memory-efficient and fast.

Key characteristics of simdjson include strict JSON compliance (with full UTF-8 validation) and automatic CPU detection to use the best parser implementation for the host at runtime.
Basic Usage Example

Include the header and use the provided classes to parse a JSON document. For instance, using the On-Demand API to retrieve a value:

#include "simdjson.h"
using namespace simdjson;

ondemand::parser parser;
padded_string json = padded_string::load("twitter.json");       // load JSON from file
ondemand::document doc = parser.iterate(json);                  // parse the JSON

// Navigate the JSON structure (On-Demand)
auto count = doc["search_metadata"]["count"].get_uint64();      // extract an integer
if (count.error()) {
    std::cerr << "Error parsing JSON: " << count.error() << "\n";
} else {
    std::cout << uint64_t(count.value()) << " results.\n";
}

In this example, the JSON file is loaded into a padded string (which simdjson requires for safety). The parser then iterates through it, and we access doc["search_metadata"]["count"] to get a nested field’s value. Casting to uint64_t gives the numeric result, which we then print. The output might be for example: “100 results.”

Note: simdjson’s On-Demand API returns a simdjson::ondemand::value or document_reference which can be cast to the expected type. Always check for errors (as shown) because the API uses a lazy parsing approach. Alternatively, the DOM API (dom::parser) can be used to parse the JSON into a dom::element that behaves more like a typical JSON DOM.
Performance

simdjson is significantly faster than many other JSON libraries (e.g., over 4× faster than RapidJSON in benchmarks). It can parse JSON at rates of multiple gigabytes per second on modern hardware, and has specialized algorithms for tasks like UTF-8 validation and number parsing. Despite its focus on speed, it does full validation of input JSON, ensuring correctness.

|]

spdlogDoc :: Text
spdlogDoc =
  [r|
spdlog

spdlog is a fast, header-only C++ logging framework. It supports logging to console and files, formatting messages with Python-like syntax, and has features like log levels, rotating logs, and asynchronous logging.
Basic Usage

Include the spdlog header and use the provided functions to log messages. The simplest usage does not require creating a logger object; you can call the global logging functions directly:

#include "spdlog/spdlog.h"

int main() {
    spdlog::info("Welcome to spdlog!"); 
    spdlog::error("Some error message with arg: {}", 1);
    
    spdlog::warn("Easy padding in numbers like {:08d}", 12);
    spdlog::critical("Support for int: {0:d}; hex: {0:x}; oct: {0:o}; bin: {0:b}", 42);
    spdlog::info("Positional args are {1} {0}..", "too", "supported");
    spdlog::info("{:<30}", "left aligned");
}

This produces output on the console with each message on a new line (info, error, warning, etc.). The {} syntax is used for formatting arguments (powered by the {fmt} library). For example, the first info logs “Welcome to spdlog!”, and the error logs “Some error message with arg: 1”. The library supports format specs like {:08d} (pad number with zeros) and positional arguments {1} and {0} as shown.

By default, the global logger logs to stdout and has a level filter (usually set to info). You can adjust the global log level with spdlog::set_level(spdlog::level::debug) to enable debug logging, and change the log pattern (format of the log line) with spdlog::set_pattern(...).
Creating Logger Instances

For more advanced usage, spdlog allows creating named loggers, including file loggers or rotating log files:

    Console logger: auto console = spdlog::stdout_color_mt("console"); creates a multi-threaded color console logger. You can then log with console->info("..."). Similarly, stderr_color_mt for stderr.
    Basic file logger: auto file_logger = spdlog::basic_logger_mt("file_logger", "logs/output.txt"); logs to a file.
    Rotating file logger: spdlog::rotating_logger_mt("rotate_logger", "logs/rot.txt", max_size, max_files); for log rotation (e.g., 5 MB each, keep 3 files).
    Daily file logger: spdlog::daily_logger_mt("daily_logger", "logs/daily.txt", 2, 30); creates a new file every day at 2:30am.

All loggers are accessible via a global registry (e.g., spdlog::get("console")) and are thread-safe by default (the “_mt” versions are multi-threaded).
Additional Features

    Log Levels: spdlog supports trace, debug, info, warn, err, critical, off. You can compile-out lower levels for performance by defining SPDLOG_ACTIVE_LEVEL.
    Backtrace: You can store a history of messages (even at trace level) and dump them on demand if something goes wrong.
    Asynchronous Logging: By using an spdlog::async_logger (or factory functions), logs can be performed in a background thread to avoid slowing down the main thread.

spdlog is highly regarded for being very fast (using efficient formatting under the hood) and easy to use, making logging almost trivial to add to any C++ project.

|]

libwebsocketsDoc :: Text
libwebsocketsDoc =
  [r|
libwebsockets

libwebsockets is a C library (usable in C++) for building WebSocket clients and servers. It’s designed to be lightweight and fast, suitable for high-performance networking scenarios.

Key points about libwebsockets:

    It operates around an event loop in a single thread. You register callback functions for various WebSocket events (like connection established, data received, writable, closed, etc.), and libwebsockets invokes these in the event loop​
    libwebsockets.org
    .
    Do not use libwebsockets APIs from multiple threads. All interactions (especially sending data) should happen in the context of its event loop thread​
    libwebsockets.org
    .
    Write (send) operations must only be done in response to a writable callback event (for flow control reasons)​
    libwebsockets.org
    ​
    stackoverflow.com
    .

Usage Overview

1. Create a context: Set up a lws_context_creation_info structure with options and create a context via lws_create_context(). This context holds server or client state.

struct lws_context_creation_info info;
memset(&info, 0, sizeof info);
info.port = CONTEXT_PORT_NO_LISTEN;      // e.g., for client (no server port)
info.iface = NULL; 
// specify protocols later (see below)
info.protocols = protocols;              // array of protocols
info.gid = -1;
info.uid = -1;
// ... (other fields like SSL certs if needed) ...

struct lws_context* context = lws_create_context(&info);
if (!context) {
    fprintf(stderr, "Websocket context create error.\n");
    return 1;
}
printf("Websocket context created.\n");

For a server, you would set info.port to a listening port (e.g., 5000)​
stackoverflow.com
​
stackoverflow.com
. For a client, use the constant CONTEXT_PORT_NO_LISTEN as shown to indicate no server socket​
stackoverflow.com
. In both cases, you provide an array of protocol definitions (see next step) and call lws_create_context​
stackoverflow.com
​
stackoverflow.com
.

2. Define protocols and callbacks: A protocol is defined by a lws_protocols struct with a name, a callback function, and per-session data size.

static int ws_service_callback(struct lws* wsi, enum lws_callback_reasons reason, void* user, void* in, size_t len) {
    switch (reason) {
        case LWS_CALLBACK_ESTABLISHED:      // server: client connected
        case LWS_CALLBACK_CLIENT_ESTABLISHED: // client: connected to server
            printf("Connection established\n");
            break;
        case LWS_CALLBACK_RECEIVE:          // server receiving data
        case LWS_CALLBACK_CLIENT_RECEIVE:   // client receiving data
            printf("Received: %s\n", (char*)in);
            // Echo the message back (for example)
            lws_callback_on_writable(wsi);  // schedule writable callback
            break;
        case LWS_CALLBACK_SERVER_WRITEABLE:
        case LWS_CALLBACK_CLIENT_WRITEABLE:
            // It's now safe to send a message
            {
                const char* msg = "Hello";
                unsigned char buf[LWS_PRE + 5];
                memcpy(buf + LWS_PRE, msg, 5);
                lws_write(wsi, buf + LWS_PRE, 5, LWS_WRITE_TEXT);
            }
            break;
        case LWS_CALLBACK_CLOSED:
            printf("Connection closed\n");
            break;
        default:
            break;
    }
    return 0;
}

struct lws_protocols protocols[] = {
    { "my-protocol", ws_service_callback, 0, 0, NULL, NULL, NULL },
    { NULL, NULL, 0, 0, NULL, NULL, NULL }  // terminator
};

This defines a single protocol "my-protocol" and its event handling callback. In the callback, based on the reason, you handle events:

    When a connection is established, you might set some state or log it​
    stackoverflow.com
    .
    On receive (LWS_CALLBACK_RECEIVE for server or LWS_CALLBACK_CLIENT_RECEIVE for client), you process incoming data​
    stackoverflow.com
    ​
    stackoverflow.com
    . (Above, we just print it and then request a writable event.)
    On writable (LWS_CALLBACK_*_WRITEABLE), you send data using lws_write()​
    libwebsockets.org
    ​
    stackoverflow.com
    . Important: You should allocate a buffer with LWS_PRE bytes of padding in front (as shown) and call lws_write() with pointer offset by that padding.
    On close, clean up or log as needed​
    stackoverflow.com
    .

Each protocol array must end with a zeroed terminator entry​
stackoverflow.com
. Pass this array to info.protocols when creating the context.

3. Connect or listen: For a server, once the context is created, libwebsockets is already listening (if a port was given). For a client, explicitly initiate a connection:

struct lws* wsi = lws_client_connect(context, "example.com", 80, 0, "/", "example.com", NULL, "my-protocol", -1);
if (!wsi) {
    printf("Client connection failed.\n");
} else {
    printf("Client connection established, wsi created.\n");
}

Here, lws_client_connect is given the address, port, optional SSL indicator (0 for plain, use LCCSCF_USE_SSL flags for SSL), the path and host, origin (NULL if not needed), the protocol name, and an ietf_version_or_minus_one (usually -1 for default)​
stackoverflow.com
.

4. Event loop: Enter a loop to service events:

while (!some_exit_flag) {
    lws_service(context, /* timeout ms = */ 50);
}
// Clean up
lws_context_destroy(context);

The lws_service(context, timeout) call will handle any pending network events (receiving, sending as needed, calling your callbacks). It returns after the given timeout if no events occur. Typically, you run this in a loop to continuously service the socket(s)​
stackoverflow.com
​
stackoverflow.com
. Set timeout to 0 for no sleep, or a number of milliseconds to wait for events.

If this is a long-running process (like a server), you might handle signals or other conditions to break out of the loop and then destroy the context. Destroying the context will close any remaining connections and free resources.

Summary: libwebsockets uses callbacks to handle all WebSocket events. Always send data in the writable callback (you can trigger one by lws_callback_on_writable() if you have data to send)​
libwebsockets.org
. The library ensures efficient I/O in one thread; if you need multi-threading, you typically have one thread generating messages and signaling the lws thread to send (via lws_callback_on_writable_all or similar) rather than calling lws_write directly across threads.

|]

cppHttplibDoc :: Text
cppHttplibDoc =
  [r|

cpp-httplib

cpp-httplib is a single-header C++11 library for HTTP(S) clients and servers. It’s easy to use: just include httplib.h (and define CPPHTTPLIB_OPENSSL_SUPPORT if HTTPS is needed) and you have a straightforward way to make HTTP requests or handle HTTP in your C++ program.
HTTP Client Example

Using the client API is straightforward:

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"

int main() {
    // Create a client for the given base URL
    httplib::Client cli("http://httpbin.org");  // or "https://..." for SSL

    auto res = cli.Get("/json");  // make a GET request to /json
    if (res) {
        std::cout << "Status: " << res->status << "\n";
        std::cout << "Body: " << res->body << "\n";
    } else {
        std::cerr << "Request failed: " << res.error() << "\n";
    }
}

In this snippet, httplib::Client is initialized with a base URL (domain). A GET request is sent to the path /json. The result res is an std::shared_ptr<httplib::Response>; if the request succeeded, you can inspect res->status (HTTP status code) and res->body (response content). If res is null, the request failed (you can retrieve an error code via res.error() in that case). The library manages the connection and will handle redirects and chunked transfer internally if needed.

According to the documentation, usage is as simple as above. For example, creating a client with a base domain and making a request requires only those two lines. You can also specify a port or use the full URL in the Client constructor.

Other HTTP methods are available as well:

    cli.Post("/path", body, "text/plain") to send a POST with a body.
    cli.Put(...), cli.Delete(...), etc., each returning a similar Response object.
    You can add headers by passing a Headers map or use the Request object for more control.

HTTPS support requires OpenSSL (define CPPHTTPLIB_OPENSSL_SUPPORT and link with OpenSSL libs). Once enabled, just use "https://..." in the URL or use httplib::SSLClient.
HTTP Server Example

cpp-httplib can also create a simple HTTP server:

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"

httplib::Server server;

server.Get("/hi", [](const httplib::Request&, httplib::Response& res) {
    res.set_content("Hello World!", "text/plain");
});

// You can set up other routes (GET, POST, etc.) similarly.

server.listen("0.0.0.0", 8080);

This code creates an HTTP server that listens on port 8080. It registers a GET handler for the path /hi – when a client requests /hi, the lambda runs and sets the response content to "Hello World!" with MIME type text/plain. The listen call starts the blocking event loop to accept requests. The server is multi-threaded by default (serving requests in parallel).

The API is reminiscent of frameworks like Express.js or Python’s Flask in its simplicity. Handlers receive a Request (which has methods to get query params, headers, etc.) and a Response to fill in.

Note: This library uses blocking I/O and is meant for simplicity. For high-performance or async needs, a non-blocking framework might be more suitable, but for many applications, cpp-httplib offers an extremely quick way to add HTTP functionality without external dependencies (just one header).
|]

doctestDoc :: Text
doctestDoc =
  [r|
doctest

doctest is a lightweight, single-header C++ testing framework. It enables writing test cases in the same source files as production code or in separate test files, with minimal overhead. Its design allows the tests to be compiled out when not needed (by not defining the implementation), making it suitable even for embedding in applications.
Getting Started

To use doctest, you typically do the following in one source file of your test suite:

    Include the doctest header and define the test runner implementation:

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

This generates a main() function that will automatically discover and run tests. (Only one source file should have this define; other test files just include doctest.h.)

Write test cases using the TEST_CASE macro:

    int factorial(int n) { return n <= 1 ? n : factorial(n-1) * n; }

    TEST_CASE("testing the factorial function") {
        CHECK(factorial(1) == 1);
        CHECK(factorial(2) == 2);
        CHECK(factorial(3) == 6);
        CHECK(factorial(10) == 3628800);
    }

    Each TEST_CASE is a standalone test. The string argument is a name/description (it can be anything, even not unique). Inside, you use CHECK() (or REQUIRE()) macros to make assertions. CHECK will log a failure but continue, whereas REQUIRE will abort the test case on failure.

Compile and run the resulting binary; it will run all test cases and report results (by default, to standard output). In the above example, the tests will fail if the factorial function is incorrect. For instance, if we forgot to handle 0, adding CHECK(factorial(0) == 1) would cause a failure and doctest would print the expression and values involved, helping identify the bug (e.g., it would show CHECK( 0 == 1 ) if factorial(0) returned 0).
Features

    Test cases are automatically registered. You don’t need to manually list them; the framework uses static initialization to discover tests.
    Subcases: You can have sub-sections in a test with the SUBCASE macro, useful for sharing setup within a test.
    Exceptions and assertions: It has macros like CHECK_THROWS to check for exceptions, CHECK_EQ for comparing values with nicer syntax, etc.
    Logging/contexts: You can add messages or context info that is reported if a test fails.
    Parameterized tests: Not built-in as a separate feature, but you can use loops or templates to generate tests.

Example Output

If a test fails, doctest prints the file and line number, the expression, and the expanded values. For example, a failing check might output:

test.cpp(10) FAILED!
  CHECK( factorial(0) == 1 )
with expansion:
  CHECK( 0 == 1 )

This clearly indicates that factorial(0) returned 0 when 1 was expected.

After running, doctest reports how many tests passed/failed and returns a non-zero code if any test failed (useful for CI).

One convenient aspect: if you want to provide your own main() (for example, to integrate tests into an existing application or to control test execution), you can omit DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN and instead call doctest::Context from your code to run tests. But for most use cases, the single #define is the quickest way to get started.

In summary, doctest lets you write tests in a very natural way with minimal setup. Its syntax is inspired by Catch2, but it's very lightweight in terms of compile times and binary size, making it practical to include even in smaller projects.
|]
