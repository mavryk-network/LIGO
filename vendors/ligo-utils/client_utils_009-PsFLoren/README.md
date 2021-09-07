# client utils 009-PsFloren

This library is a very small portion of the library tezos-client-009-PsFloren with the goal of cutting out heavy dependencies that are pulled in.

When we want to upgrade the protocol to a new version we should copy the contents of the files with the same name as here.

There are some minor additions to each file to keep the dependency cone small, these functions and values are added before any `open` statement.