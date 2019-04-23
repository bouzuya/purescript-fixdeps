module Main
  ( main
  ) where

import Prelude

import Data.Array (scanl)
import Data.Array as Array
import Data.Array as Foldable
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff as Exception
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.Process as Process
import Node.Stream as Stream
import Partial.Unsafe as Unsafe
import Simple.JSON as SimpleJSON

type SpagoPackage =
  { name :: String
  , dependencies :: Array String
  , packages :: Object { dependencies :: Array String }
  }
type Package = Tuple String (Set String)

readStdin :: Aff String
readStdin =
  let r = Process.stdin
  in
    Aff.makeAff
      (\callback -> do
        ref <- Ref.new ""
        Stream.onDataString r Encoding.UTF8 \s -> do
          buffer <- Ref.read ref
          Ref.write (buffer <> s) ref
        Stream.onEnd r do
          buffer <- Ref.read ref
          callback (pure buffer)
        pure mempty)

flattenDeps :: String -> Object { dependencies :: Array String } -> Set String
flattenDeps name packages =
  case Object.lookup name packages of
    Maybe.Nothing -> Unsafe.unsafeCrashWith ("unknown package : " <> name)
    Maybe.Just package ->
      (Set.singleton name)
      <> Foldable.fold (map ((flip flattenDeps) packages) package.dependencies)

toPackage :: String -> Object { dependencies :: Array String } -> Package
toPackage name packages = Tuple.Tuple name (flattenDeps name packages)

simplify :: SpagoPackage -> SpagoPackage
simplify package =
  let
    packages' :: Array Package
    packages' = map (flip toPackage package.packages) package.dependencies

    dependencies' :: Array String
    dependencies' =
      Array.filter
        (\name ->
          let
            others = Array.filter ((notEq name) <<< Tuple.fst) packages'
            othersDeps = Array.foldMap Tuple.snd others
          in
            not (Set.member name othersDeps))
        package.dependencies
  in
    { name: package.name
    , dependencies: dependencies'
    , packages: package.packages
    }

main :: Effect Unit
main = Aff.launchAff_ do
  input <- readStdin
  package <-
    Either.either
      (\s -> Exception.throwError (Aff.error (show s)))
      pure
      ((SimpleJSON.readJSON input) :: _ _ SpagoPackage)
  Console.logShow (simplify package).dependencies
