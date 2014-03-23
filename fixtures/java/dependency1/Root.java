import net.abc.*;
import net.foo.Solaris;
import net.foo.alpha.*;
import java.lang.*;

public interface Root {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
