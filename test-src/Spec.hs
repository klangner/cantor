import Test.Hspec
import Test.QuickCheck
import qualified Utils.FolderSpec
import qualified Project.MavenSpec
import qualified AST.JavaParserSpec


main :: IO ()
main = hspec $ do
  describe "Folder" Utils.FolderSpec.spec
  describe "Maven" Project.MavenSpec.spec
  describe "JavaParser" AST.JavaParserSpec.spec