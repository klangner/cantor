import Test.Hspec
import Test.QuickCheck
import qualified Utils.FolderSpec
import qualified Project.MavenSpec
import qualified AST.Parser.JavaParserSpec
import qualified AST.DependencySpec
import qualified Project.SourcesSpec
import qualified Metric.BasicSpec


main :: IO ()
main = hspec $ do
  describe "Folder utilities" Utils.FolderSpec.spec
  describe "Project.Maven" Project.MavenSpec.spec
  describe "Project sources" Project.SourcesSpec.spec
  describe "Java AST Parser" AST.Parser.JavaParserSpec.spec
  describe "Dependency analysis" AST.DependencySpec.spec
  describe "Basic metrics" Metric.BasicSpec.spec
  