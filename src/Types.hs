module Types (FunctionSignature, FunctionName, Type, ModuleName) where

type FunctionSignature = (ModuleName, FunctionName, Type)
type FunctionName = String
type Type = String
type ModuleName = String
