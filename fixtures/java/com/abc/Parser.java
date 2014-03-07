package com.abc;

import com.abc.model.Package;


public interface Parser {

	public IPackage parseFile(String fileName);
	public IPackage parsePackage(String packagePath);
	public IPackage parseProject(String srcPath);
}
