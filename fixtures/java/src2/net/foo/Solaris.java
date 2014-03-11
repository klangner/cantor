package net.foo;

/* Imports go here */
import com.abc;
// Some comment
import net.foo.model.*;
import java.lang.*;

public interface Solaris {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
