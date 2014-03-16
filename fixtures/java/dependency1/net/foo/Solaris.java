package net.foo;

/* Imports go here */
import net.abc.*;
// Some comment
import net.foo.*;
import java.lang.*;

public interface Solaris {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
