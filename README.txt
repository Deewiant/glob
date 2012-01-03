This is Glob, a Haskell library for globbing, i.e. pattern matching file paths
akin to the POSIX glob() function. Haddock documentation is included, and can
be built with the 'cabal haddock' command. Basic usage info is repeated below:

Matching pattern (a String) against filepath (a FilePath):

	match (compile pattern) filepath

Matching a pattern against all paths in the current working directory:

	glob pattern

Matching a pattern against all paths in a given directory (a FilePath):

	globDir1 (compile pattern) directorypath

Matching a list of patterns against all paths in a given directory, returning
the matches for each pattern as well as the paths not matched by any of the
patterns:

	globDir (map compile patterns) directorypath
