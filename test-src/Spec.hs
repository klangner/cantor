import Test.Hspec
import Test.QuickCheck
import qualified Cantor.Utils.FolderSpec
import qualified Cantor.Language.Java.MavenSpec
import qualified Cantor.Language.Java.ParserSpec
import qualified Cantor.Language.Java.ProjectSpec
import qualified Cantor.Metric.BasicSpec


main :: IO ()
main = hspec $ do
  describe "Folder utilities" Cantor.Utils.FolderSpec.spec
  describe "Project.Maven" Cantor.Language.Java.MavenSpec.spec
  describe "Project sources" Cantor.Language.Java.ProjectSpec.spec
  describe "Java AST Parser" Cantor.Language.Java.ParserSpec.spec
  describe "Basic metrics" Cantor.Metric.BasicSpec.spec
  
