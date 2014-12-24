import Test.Hspec
import Test.QuickCheck
import qualified Cantor.Utils.FolderSpec
import qualified Cantor.Parser.JavaSpec


main :: IO ()
main = hspec $ do
  describe "Folder utilities" Cantor.Utils.FolderSpec.spec
  describe "Java AST Parser" Cantor.Parser.JavaSpec.spec

