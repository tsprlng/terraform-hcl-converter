{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TerraformJson where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.List (find, groupBy, isPrefixOf, sortOn)
import Data.Text (pack)
import TerraformParser (TfDeclaration (..), TRVal (..))

instance ToJSON TfDeclaration where
  toJSON (TfConfig decls) = _json decls
  toJSON (TfProvider _ decls) = _json decls
  toJSON (TfResource _ _ decls) = _json decls
  toJSON (TfData _ _ decls) = _json decls
  toJSON (TfOutput _ decls) = _json decls
  toJSON (TfVariable _ decls) = _json decls

instance ToJSON TRVal where
  toJSON (TStr s) = toJSON $ pack s
  toJSON (TBool b) = toJSON b
  toJSON (TInt i) = toJSON i
  toJSON (TMap decls) = _json decls
  toJSON (TArray decls) = toJSON decls

_json :: (ToJSON v)=> [(String, v)] -> Value
_json decls = object $ concat [backends, provisioners, notSpecial]
  where
    -- decided to handle these annoying syntactic special cases by encoding them as special keys starting with __
    --
    provisioners = if null _provisioners then [] else [pack "provisioner" .= (map (_json . (:[])) _provisioners)]
    _provisioners = map (\(k,v)-> (drop 14 k, v)) $ filter (\(k,v)-> "__provisioner_" `isPrefixOf` k) decls
    backends = if null _backends then [] else [pack "backend" .= _json _backends]
    _backends = map (\(k,v)-> (drop 10 k, v)) $ filter (\(k,v)-> "__backend_" `isPrefixOf` k) decls

    notSpecial = map (\(k,v)-> pack k .= v) $ filter (\(k,v)-> not $ or $ map (`isPrefixOf` k) ["__provisioner_","__backend_"]) decls

tfToJson :: [TfDeclaration] -> ByteString
tfToJson decls = encodePretty $ object $ concat [
    if null terraconfs then [] else ["terraform" .= terraconfs],
    if null providers then [] else ["provider" .= providers],
    if null resources then [] else ["resource" .= (_json . map (\(a,b)-> (a, _json b)) . tree) resources],
    if null datas then [] else ["data" .= (_json . map (\(a,b)-> (a, _json b)) . tree) datas],
    if null outputs then [] else ["output" .= _json outputs],
    if null variables then [] else ["variable" .= _json variables]
  ]
  where
    terraconfs :: [Value]
    terraconfs = concatMap _tco decls  -- just splat everything together (TODO actually i'm not sure about this one, but it seems to work...)
    providers :: [Value]
    providers = concatMap _pro decls  -- weird list of objects like [{"aws":{...}},{"aws":{...}}] to allow multiple aliases
    resources :: [(String, (String, Value))]
    resources = concatMap _res decls  -- list with twin keys and value, so it can be flattened into a tree above
    datas :: [(String, (String, Value))]
    datas = concatMap _data decls  -- same as resources
    outputs :: [(String, Value)]
    outputs = concatMap _out decls  -- straightforward (non-tree-shaped) key/value list
    variables :: [(String, Value)]
    variables = concatMap _var decls  -- same as outputs
    _tco (TfConfig dd) = [_json dd]
    _tco _ = []
    _pro (TfProvider k dd) = [_json [(k, _json dd)]]
    _pro _ = []
    _res (TfResource t k dd) = [(t, (k, _json dd))]
    _res _ = []
    _data (TfData t k dd) = [(t, (k, _json dd))]
    _data _ = []
    _out (TfOutput k dd) = [(k, _json dd)]
    _out _ = []
    _var (TfVariable k dd) = [(k, _json dd)]
    _var _ = []

    tree :: (Ord a, Eq a)=> [(a,b)]->[(a,[b])]
    tree = join . groupBy (\a b -> fst a == fst b) . sortOn fst
    join :: [[(a,b)]] -> [(a,[b])]
    join = map (\xs-> (fst $ head xs, map snd xs))
