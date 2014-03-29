import Test.Hspec
import Test.QuickCheck
import qualified Utils.FolderSpec
import qualified Project.MavenSpec
import qualified AST.JavaParserSpec
import qualified Project.JavaSpec
import qualified Metric.BasicSpec


main :: IO ()
main = hspec $ do
  describe "Folder utilities" Utils.FolderSpec.spec
  describe "Project.Maven" Project.MavenSpec.spec
  describe "Project sources" Project.JavaSpec.spec
  describe "Java AST Parser" AST.JavaParserSpec.spec
  describe "Basic metrics" Metric.BasicSpec.spec
  