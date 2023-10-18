module Main where

import Prelude hiding (max)

import Data.Array (index, (..))
import Data.Foldable (traverse_)
import Data.Int (fromStringAs)
import Data.Int as Radix
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, toUpper)
import Data.String as String
import Data.String.CodeUnits (slice)
import Effect (Effect)
import Effect.Aff (launchAff_, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Fmt (fmt)
import Node.Buffer (fromString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, rm', writeFile)
import Node.Path (FilePath)

main :: Effect Unit
main = do
  launchAff_ generate

generate :: forall m. MonadAff m => m Unit
generate = do
  void $ liftAff $ try $ rm' "./packages" { recursive: true, force: false, maxRetries: 0, retryDelay: 0 }
  liftAff $ mkdir "./packages"
  traverse_ (\name -> do
    let pursName = toPursName name
        dir = "./packages/" <> name
        src = dir <> "/src"
        test = dir <> "/test"
        mainDir = src <> "/" <> pursName
        spagoFile = dir <> "/spago.yaml"
        mainFile = mainDir <> "/Main.purs"
        testFile = test <> "/Main.purs"

    liftAff $ mkdir dir
    liftAff $ mkdir src
    liftAff $ mkdir test
    liftAff $ mkdir mainDir

    stringFile spagoFile (spagoYaml name)
    stringFile mainFile (mainPurs pursName)
    stringFile testFile (testMainPurs pursName)
  ) oneHundredPackageNames

stringFile :: forall m. MonadAff m => FilePath -> String -> m Unit
stringFile path file = do
  buffer <- liftEffect $ fromString file UTF8
  liftAff $ writeFile path buffer

oneHundredPackageNames :: Array String
oneHundredPackageNames = do
  number <- 1..max
  pure $ "package-" <> (show number)

max :: Int
max = 100

dependencies :: String -> String
dependencies name =
  let number = join $ split (Pattern "-") name # flip index 1 <#> fromStringAs Radix.decimal
  in
  case map ((+) 1) number of
    -- Each package dependent on the last
    -- Just n | n > max -> ""
    -- Just n -> "    - package-" <> show n

    -- All but the first 10 dependent on the first 10
    Just n | n <= 11 -> ""
    Just _ -> joinWith "\n" $ 1..10 <#> \n -> "    - package-" <> show n
    _ -> ""

spagoYaml :: String -> String
spagoYaml name =
  fmt
    @"""package:
  dependencies:
    - console
    - effect
    - fmt
    - prelude
    - aff
    - node-fs
    - node-buffer
    - halogen
    - halogen-formless
    - halogen-vdom
    - halogen-hooks
    - halogen-vdom-string-renderer
    - homogeneous
    - heterogeneous
{dependencies}
  name: {name}
  test:
    dependencies:
      - spec
    main: Test.Main
    """
    { name: name
    , dependencies: dependencies name
    }

mainPurs :: String -> String
mainPurs name =
  fmt
    @"""module {name}.Main where

import Prelude

import Effect (Effect)

main :: Effect Unit
main = pure unit
"""
  {name: firstUpper name}

testMainPurs :: String -> String
testMainPurs name =
  fmt
    @"""module Test.{name}.Main where

import Prelude

import Effect (Effect)

main :: Effect Unit
main = pure unit
"""
  {name: firstUpper name}

toPursName :: String -> String
toPursName = split (Pattern "-") >>> map firstUpper >>> joinWith ""

firstUpper :: String -> String
firstUpper s = toUpper (slice 0 1 s) <> slice 1 (String.length s) s

