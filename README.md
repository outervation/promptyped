**Promptyped**

This repository provides a structured way to integrate Large Language Models (LLMs) into typed workflows; a preliminary, embedded implementation of a NOn-deterministic exTremely-high-level Programming Language, (a NOTPL). It achieves this via typed prompts/responses, tools (functions the model can call), smart context management and a build/test system abstraction. The goal is to guide an LLM through multi-step tasks, ensuring outputs conform to expected types, source files compile and unit tests pass, and allowing the LLM to “call” predefined tools to manipulate files or perform other actions necessary for task completion.

There's an initial writeup at: https://outervationai.substack.com/p/getting-an-llm-to-write-a-small-nontrivial . And a writeup of recent improvements at: https://outervationai.substack.com/p/building-a-100-llm-written-standards .

Examples of usage are in the app dir, i.e. app/TaskFromConfig.hs. It might be used like `promptyped ./config_app_example_big_task.json ./config_model_example.json`. Currently supports Go and Python (via `uv`) as project languages, but the Python support isn't well tested.

The workflows/prompts are in src/PromptCommon.has.

The main interface to the library is the following:

```
runAiFunc ::
  forall bs a b.
  (FromJSON a, ToJSON a, Show a, BS.BuildSystem bs) =>
  Context ->
  IntelligenceRequired ->
  [Tools.Tool] ->
  a ->
  (a -> AppM (Either (MsgKind, Text) b)) ->
  RemainingFailureTolerance ->
  AppM b
```
* a is a type parameter, the type of the object we want the LLM to return, which must be convertible to JSON.
* bs is a type representing the build system (essentially an interface), which allows changing the backend used for compiling and running unit tests. 
* Context is a struct with the background text for the task, and [Tools.Tool] is a list of tools that task is allowed to use. Currently the library doesn’t support externally defined tools, but new tools can easily be added to the library in Tools.hs.
* IntelligenceRequired is an enum: HighIntelligenceRequired | MediumIntelligenceRequired | LowIntelligenceRequired . It allows using different models for different parts of the pipeline; the models are specified in the config.
* We provide a value of type a to the function as an example/dummy value, to illustrate to the LLM how it should look.
* RemainingFailureTolerance is just an integer representing how many syntax errors (LLM returns syntactically incorrect JSON etc.) before aborting.
* The most complex type is (a -> AppM (Either (MsgKind, Text) b)). This is the type of a validator: a function that takes an a as input, and returns either a Text error and error kind, or a value of type b. The result is however wrapped in AppM, a monad, which just means it has access to a state and the ability to do IO; e.g. to validate that a file really exists on disk, or that compilation succeeds and unit tests pass. 
* The runAiFunc function will keep looping until the LLM returns a value for which the validator passes (or RemainingFailureTolerance reaches zero).

License is Apache 2.0. Contributions are welcome, but for significant changes please check with me first WRT whether it matches the intended direction of the library.

Still heavily WIP so no guarantees of stability or correctness.

Planned future tasks include:
* Adding support for focusing/unfocusing individual functions, rather than just files, to allow worker with larger files better.
* Adding support for multiple simultaneous tasks.
* Adding support for Haskell as a project language, so promptyped can be used to write promptyped workflows. 
* Cleaning up the Haskell code and making AppM a free monad, so the library can be unit tested.
