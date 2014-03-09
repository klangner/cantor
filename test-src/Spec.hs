import Test.Hspec
import Test.QuickCheck
import qualified Utils.FolderSpec
import qualified Project.MavenSpec
import qualified AST.JavaParserSpec
import qualified Project.SourcesSpec


main :: IO ()
main = hspec $ do
  describe "Utils.Folder" Utils.FolderSpec.spec
  describe "Project.Maven" Project.MavenSpec.spec
  describe "ProjectSources" Project.SourcesSpec.spec
  describe "AST.JavaParser" AST.JavaParserSpec.spec
