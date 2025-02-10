# Did you find a bug?

- Please create a new issue with description.
- It would be nice if you provide more details such as GHC version, cabal-install version (or other build tool title and version).
- Reproducible case will be appreciated.

# Do you want to add a new or missing encoding?

1. Add file to repo (see `.mapping` or `.xml` files in `./Data/Encoding`).
2. Install encoding generator via `cabal install -fgenerate-encodings encoding-exe`.
3. Run encoding generator with your newly added files.
4. List generated modules in `encoding.cabal` and commit file.
5. Ensure the package could be built.
