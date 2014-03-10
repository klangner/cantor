# Usage

## Find project source paths

To find all source path use command:

```shell
cantor <project_src>
```

It will find all source path in subfolders of <project_path>.
This command works with Java files only.

####Example

```shell
cantor fixtures/java
```

Will print:

```shell
/home/klangner/workspaces/haskell/cantor/fixtures/java/multi1/src1
/home/klangner/workspaces/haskell/cantor/fixtures/java/multi1/src2
/home/klangner/workspaces/haskell/cantor/fixtures/java/src1
```

## Lines of code (LOC) metric

To count number of lines in a given project use command:

```shell
cantor loc <project_src>
```

Program will count lines in Java, Haskell, JavaScript, Ruby and Python source files.

#### Example

```shell
cantor loc fixtures/java
```

Will print:

```shell
.java lines: 72
.hs lines: 654
```

