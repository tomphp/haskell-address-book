{-# LANGUAGE OverloadedStrings #-}

module Domain.ContactSpec where

import Test.Hspec

import qualified Domain.Contact as Contact

spec :: Spec
spec =
    describe "Contact" $
        describe "new" $ do
            it "returns the created contact" $ do
                let contact = Contact.new "Tom" "01234567890"

                fmap Contact.name contact   `shouldBe` Right "Tom"
                fmap Contact.number contact `shouldBe` Right "01234567890"

            it "returns error if name is empty" $
                Contact.new "" "0123456789" `shouldBe` Left "Name must be present."

            it "returns error if number is empty" $
                Contact.new "Tom" "" `shouldBe` Left "Number must be present."
