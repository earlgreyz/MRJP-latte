# Compiler construction (MRJP) lab project

Laboratory grade is based on a project involving the implementation of a compiler for a simple
imperative language [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/Latte/description.html).

## Stages

The project consits of three stages:

1. Front-end: syntactic and semantic analysis - should be finished __until Dec 9__, but there is no
 separate moodle assignment for it.
2. LLVM or x86 back-end. __Deadline Jan 7__.
3. (optional) extensions. __Deadline Jan 20__.

Your lab supervisor can set slightly different deadline, in particular require project presentation
in person. Nonetheless, all projects must be submitted to moodle.

### Scoring

At most 24 points in total, counting towards final course mark. To pass the lab and qualify to sit
the exam, it is required to submit satisfactory solutions for all stages and get a total of at
least 15 points _(including points for the Instant compiler)_.

Points are awarded for:

1. front-end (4);
2. back-end for LLVM (8) or (exclusive) x86 (10)
3. LLVM code in SSA form - additional 1p
4. register allocation for x86 - additional (up to) 4p
5. optimisations - up to 3p
6. Extension (x/y denotes points for LLVM/x86 respectively)
7. arrays (2)
 * structures (2)
 * objects (attributes, methods, inheritance without method override) - (3/4) extra
 * virtual methods (3/4) extra (in total 8/10 can be awarded for objects with virtual methods)
 * garbage collection (2)

_A more precise specification of extensions is contained in the language description._

### Late submissions

Late submissions are subject to penalty of 2p for each week started; however no submissions will
be accepted after __February 7__.

## Rules

The project must be original work, developed independently by the student submitting it. In
particular:

* you must not look at other students’ code, show or share your code in any manner.
* any foreign code (e.g. from books, tutorials, or other sources) must be clearly marked and
attributed.

### Technical requirements

1. The project must be submitted as a packed TAR archive (.tar.gz, .tgz, tar.bz2 or .tbz)
2. The project root must contain at least:
 * Text file __README__ describing how to compile and run the project, used tools and libraries,
  implemented extensions, structure of the project, pointers to more detailed documentation.
 * __Makefile__ allowing to build the program
 * A directory __src__ containing only source files of the project (possibly including Latte.cf,
  makefiles etc.); auxiliary files such as libraries, etc should be placed in other directories.
3. The project must be buildable on lab computers by running make in the project root.
4. All necessary libraries (except standard library of the programming language used) must be
 described in README.
5. After the build, project root must contain an executable file __latc__ (may be a shell script  
 calling other programs)
6. The compiler must accept all test programs from the directory __good__ and reject (with
 appropriate error messages) all programs from the directory __bad__. For the extensions, the
 compiler must accept all programs from the respective subdirectories in extension. Execution of a
 correct test program must give output exactly the same as in the corresponding __.output__ file
 (given input in the corresponding __.input__ file, if any).
7. For an accepted program, the compiler must output OK (`"OK\n"`) and exit with code 0.
8. For a rejected program, the first line of stderr must be ERROR (`"ERROR\n"`). Further lines
 should contain appropriate error messages. The compiler must then exit with a non-zero exit code.

#### LLVM backend

1. After running `make` the project root should contain executable __latc_llvm__
2. Running `latc_llvm foo/bar/baz.lat` for a correct program __baz.lat__ should create files
 __baz.ll__ (readable LLVM code) and executable __a.out__ in the directory __foo/bar__.
3. Helper functions (`printInt` etc.) should be placed in the file __runtime.bc__ in the __lib__
 directory (with sources in __runtime.ll__).

#### x86 backend

1. After running `make` the project root should contain executable __latc_ARCH__ where _ARCH_ is
 _x86_ or _x86_64_
2. Running `latc_ARCH foo/bar/baz.lat` for a correct program __baz.lat__ should create files
 __baz.s__ (assembly) and executtable __a.out__ in the directory __foo/bar__.
3. Helper functions (`printInt` etc.) should be placed in the file __runtime.o__ in the __lib__
 directory (with sources in __runtime.s__).

#### Test programs archive:

[lattests121017.tgz](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/Latte/lattests121017.tgz),
MD5:49ca5702ca9795fb8f33d52b0b3c6fc3
