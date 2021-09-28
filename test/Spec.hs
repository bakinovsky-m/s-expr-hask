import Test.Hspec
import Lib


main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "evaluate value s-expr" $
      (eval . SValue $ Value 123) `shouldBe` 123

    it "evaluate recursive s-expr" $
      eval (SOp Plus (SValue $ Value 3) (SValue $ Value 1)) `shouldBe` 4

  describe "parse_and_eval" $ do
    it "parses valid value s-expr" $
      parse_and_eval sexpr "123" `shouldBe` Right 123
    it "parses valid recursive s-expr" $
      parse_and_eval sexpr "(+ 123 321)" `shouldBe` Right (123 + 321)
    it "fails on invalid strings" $
      parse_and_eval sexpr "(" `shouldSatisfy` either (const True) (const False)
