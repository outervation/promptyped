{-# LANGUAGE OverloadedStrings #-}

module ShapeChecker (checkShapesMatch) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List ((\\))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude

data PathElement
  = ObjectKey Text
  | ArrayIndex Int
  deriving (Show)

type Path = [PathElement]

indent :: Text -> Text
indent = unlines . map ("    " <>) . lines

typeName :: Value -> Text
typeName (Object _) = "Object"
typeName (Array _) = "Array"
typeName (String _) = "String"
typeName (Number _) = "Number"
typeName (Bool _) = "Bool"
typeName Null = "Null"

describeArrays :: (Semigroup a1, IsString a1, Show a2, Show a3) => a2 -> a3 -> a1
describeArrays len1 len2 =
  "First array has "
    <> show len1
    <> " elements, "
    <> "second array has "
    <> show len2
    <> " elements"

formatKeys :: [Key.Key] -> Text
formatKeys = T.intercalate ", " . map Key.toText . sort

checkShapesMatch :: Value -> Value -> Either Text ()
checkShapesMatch = checkShape []

checkShape :: Path -> Value -> Value -> Either Text ()
checkShape path val1 val2 = case (val1, val2) of
  (Object o1, Object o2) ->
    let keys1 = KM.keys o1
        keys2 = KM.keys o2
        missing = keys1 \\ keys2
        extra = keys2 \\ keys1
     in if not (null missing && null extra)
          then
            Left
              $ "Object structure mismatch at "
              <> formatPath path
              <> ":\n"
              <> "  Missing required fields: "
              <> formatKeys missing
              <> "\n"
              <> "  Unexpected extra fields: "
              <> formatKeys extra
          else
            foldM_
              ( \key -> case (KM.lookup key o1, KM.lookup key o2) of
                  (Just v1, Just v2) -> checkShape (ObjectKey (Key.toText key) : path) v1 v2
                  _ ->
                    Left
                      $ "Failed to access required field '"
                      <> Key.toText key
                      <> "' at "
                      <> formatPath path
              )
              (sort keys1)
  (Array a1, Array a2) -> do
    checkArrayHomogeneity path a1 "First array"
    checkArrayHomogeneity path a2 "Second array"
    if V.null a1 && V.null a2
      then Right ()
      else
        if V.null a1 || V.null a2
          then
            Left
              $ "Array length mismatch at "
              <> formatPath path
              <> ": "
              <> describeArrays (V.length a1) (V.length a2)
          else checkShape (ArrayIndex 0 : path) (V.head a1) (V.head a2)
  (String _, String _) -> Right ()
  (Number _, Number _) -> Right ()
  (Bool _, Bool _) -> Right ()
  (Null, Null) -> Right ()
  _ ->
    Left
      $ "Type mismatch at "
      <> formatPath path
      <> ":\n"
      <> "  Expected: "
      <> typeName val1
      <> "\n"
      <> "  Found: "
      <> typeName val2

checkArrayHomogeneity :: Path -> Vector Value -> Text -> Either Text ()
checkArrayHomogeneity path vec arrayName
  | V.null vec = Right ()
  | otherwise = do
      let firstOne = V.head vec
          rest = V.tail vec
          firstType = typeName firstOne
      V.imapM_
        ( \i v ->
            case checkShape (ArrayIndex i : path) firstOne v of
              Left err ->
                Left
                  $ "Array type inconsistency in "
                  <> arrayName
                  <> " at "
                  <> formatPath path
                  <> ":\n"
                  <> "  - Expected type '"
                  <> firstType
                  <> "' (from element 0)\n"
                  <> "  - Found different type at index "
                  <> show (i + 1)
                  <> ":\n"
                  <> indent err
              Right _ -> Right ()
        )
        rest

formatPath :: Path -> Text
formatPath [] = "root"
formatPath path = T.intercalate "." . reverse $ map formatElement path
  where
    formatElement (ObjectKey key) = key
    formatElement (ArrayIndex i) = "[" <> show i <> "]"

foldM_ :: (a -> Either Text ()) -> [a] -> Either Text ()
foldM_ f = foldr (\x acc -> acc *> f x) (Right ())

main :: IO ()
main = do
  let badArray = Array $ V.fromList [Number 1, String "bad", Bool True]
      goodArray = Array $ V.fromList [Number 2, Number 3, Number 4]
      goodArray2 = Array $ V.fromList [String "str", String "str"]

  putTextLn "Test 1: Both arrays homogeneous but mismatched:"
  case checkShape [] goodArray goodArray2 of
    Left err -> putTextLn err
    Right _ -> putTextLn "Unexpected success"

  putTextLn "\nTest 2: Heterogeneous array detection:"
  case checkShape [] badArray badArray of
    Left err -> putTextLn err
    Right _ -> putTextLn "Unexpected success"
