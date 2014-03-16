import net.abc.*;
import net.foo.Solaris;
import java.lang.*;

public interface Root {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
