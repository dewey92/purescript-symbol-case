module Test.Main where

import Prelude

import Data.Symbol (SProxy(..), reflectSymbol)
import Data.Symbol.Case as Case
import Effect (Effect)
import Effect.Aff (launchAff_)
import Record.Case (toCamelRecord)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Symbol.Case" do
    describe "to lower case" do
      it "accepts empty symbol" do
        let emptyP = SProxy :: SProxy ""
        (reflectSymbol $ Case.toLower emptyP) `shouldEqual` ""
      it "keeps single lowercase letter" do
        let a = SProxy :: SProxy "a"
        (reflectSymbol $ Case.toLower a) `shouldEqual` "a"
      it "converts an uppercase letter" do
        let a = SProxy :: SProxy "A"
        (reflectSymbol $ Case.toLower a) `shouldEqual` "a"
      it "keeps non-alpha characters" do
        let nonAlpha = SProxy :: SProxy "$#_+ &"
        (reflectSymbol $ Case.toLower nonAlpha) `shouldEqual` "$#_+ &"
      it "converts all combinations to lowercase" do
        let johnDoe = SProxy :: SProxy "$$John Doe"
        (reflectSymbol $ Case.toLower johnDoe) `shouldEqual` "$$john doe"

    describe "to upper case" do
      it "accepts empty symbol" do
        let emptyP = SProxy :: SProxy ""
        (reflectSymbol $ Case.toUpper emptyP) `shouldEqual` ""
      it "keeps single uppercase letter" do
        let a = SProxy :: SProxy "A"
        (reflectSymbol $ Case.toUpper a) `shouldEqual` "A"
      it "converts a lowercase letter" do
        let a = SProxy :: SProxy "a"
        (reflectSymbol $ Case.toUpper a) `shouldEqual` "A"
      it "keeps non-alpha characters" do
        let nonAlpha = SProxy :: SProxy "$#_+ &"
        (reflectSymbol $ Case.toUpper nonAlpha) `shouldEqual` "$#_+ &"
      it "converts all combinations to lowercase" do
        let johnDoe = SProxy :: SProxy "$$John Doe"
        (reflectSymbol $ Case.toUpper johnDoe) `shouldEqual` "$$JOHN DOE"

    describe "to pascal case" do
      it "accepts empty symbol" do
        let emptyP = SProxy :: SProxy ""
        (reflectSymbol $ Case.toPascal emptyP) `shouldEqual` ""
      it "keeps single uppercase letter" do
        let a = SProxy :: SProxy "A"
        (reflectSymbol $ Case.toPascal a) `shouldEqual` "A"
      it "converts a lowercase letter" do
        let a = SProxy :: SProxy "a"
        (reflectSymbol $ Case.toPascal a) `shouldEqual` "A"
      it "dismisses non-alpha characters" do
        let nonAlpha = SProxy :: SProxy "$#_+ &a"
        (reflectSymbol $ Case.toPascal nonAlpha) `shouldEqual` "A"
      it "converts all combinations to pascalcase" do
        let johnDoe = SProxy :: SProxy "$$John Doe"
        (reflectSymbol $ Case.toPascal johnDoe) `shouldEqual` "JohnDoe"

    describe "to camel case" do
      it "accepts empty symbol" do
        let emptyP = SProxy :: SProxy ""
        (reflectSymbol $ Case.toCamel emptyP) `shouldEqual` ""
      it "keeps single uppercase letter" do
        let a = SProxy :: SProxy "A"
        (reflectSymbol $ Case.toCamel a) `shouldEqual` "a"
      it "converts a lowercase letter" do
        let a = SProxy :: SProxy "a"
        (reflectSymbol $ Case.toCamel a) `shouldEqual` "a"
      it "dismisses non-alpha characters" do
        let nonAlpha = SProxy :: SProxy "$#_+ &a"
        (reflectSymbol $ Case.toCamel nonAlpha) `shouldEqual` "a"
      it "converts all combinations to camelcase" do
        let johnDoe = SProxy :: SProxy "$$John Doe"
        (reflectSymbol $ Case.toCamel johnDoe) `shouldEqual` "johnDoe"

  describe "Record.Case" do
    it "converts record keys to camelcase" do
      let rec = { movie_genre: "Horror", actor: "John" }
      (toCamelRecord rec) `shouldEqual` { movieGenre: "Horror", actor: "John" }
