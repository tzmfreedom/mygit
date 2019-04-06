import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "searchFile" $ do
    it "" $ do
      let result = searchFile "foo" [defaultObject]
      result `shouldBe` True

    it "" $ do
      let result = searchFile "bar" [defaultObject]
      result `shouldBe` False

  describe "replaceTree" $ do
    it "replace file" $ do
      let result = replaceTree [defaultObject] defaultObject{objectHash = "baz"} ["foo"]
      result `shouldBe` [defaultObject{objectHash = "baz"}]

    it "append file" $ do
      let result = replaceTree [defaultObject] defaultObject{objectHash = "bar"} ["foo2"]
      result `shouldBe` [
        defaultObject{objectName = "foo2", objectHash = "bar"},
        defaultObject
        ]

    it "append tree" $ do
      let result = replaceTree [] defaultObject{objectName = "foo/foo"} ["foo", "foo"]
      result `shouldBe` [defaultObject']

    it "replace tree" $ do
      let result = replaceTree [defaultObject'] defaultObject{objectName = "foo/foo", objectHash = "baz"} ["foo", "foo"]
      result `shouldBe` [
        defaultObject'{
          objectHash = "44EA27AEA49FF0E69DB7329FFC8530D0A80060E4",
          objectChildren = [defaultObject{objectHash = "baz"}]
          }
        ]

defaultObject :: Object
defaultObject = do
  Object{
    objectType = "file",
    objectName = "foo",
    objectHash = "bar",
    objectPerm = [7, 5, 5],
    objectChildren = []
    }

defaultObject' :: Object
defaultObject' = do
  Object{
    objectType = "tree",
    objectName = "foo",
    objectHash = "FFBBC3A59F97291C2C20F909BEA3E947E91D9230",
    objectPerm = [7, 5, 5],
    objectChildren = [defaultObject]
    }
