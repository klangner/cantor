[![Build Status](https://travis-ci.org/klangner/cantor.svg?branch=master)](https://travis-ci.org/klangner/cantor)

# Cantor

Cantor is application for analyzing software projects.
The goal of this application is to help developer understand source code of unknown project.

Currently this application will show the following information:
 * What language is this application written in
 * Line of code metric to assess project size
 * What build system is used (Maven, Rake etc)


## Installation
The library can be installed from [Hackage](http://hackage.haskell.org/package/cantor) with the command:

```sh
cabal install cantor
```

## Sample Usage

```sh
cantor <project_source_path>
```

## Architecture

Folders:

 * Cantor.Build - Contains parsers for various build systems (SBT, Cabal, Maven, etc)
 * Cantor.Parser - Parsers for various source files (Haskell, Scala, Java)
 * Cantor.Analysis - Analysis
 * Cantor.Utils - Utility functions
 

## Redistributing
This code is distributed under BSD3 License. It may be freely redistributed, subject to the provisions of this license.