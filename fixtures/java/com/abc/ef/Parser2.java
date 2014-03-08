/* Multi line comment */
// single line comment
package com.abc.ef;

import com.abc.model.Package;


public interface Parser2 {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
