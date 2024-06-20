# azure-haskell

## Purpose

This repository provides functions and types for common azure related
operations. This is very much incomplete but serves as a boilerplate to get
started. Covered areas:
- Authentication (with managed identities only as of now)
- Key Vault
- Blob storage (To be pushed)
- Email communication service (To be pushed)

## Building the project

To build the entire project, run:
```
stack build
```

In order to build individual components of the library, `cd` into the package and run:
```
cabal build
```
