# trivial-package-local-nicknames

This was done so there is a portability library for the PLN API not included in `DEFPACKAGE`.

## Supported implementations

* SBCL
* CCL
* ECL
* Clasp
* ABCL
* ACL
* LispWorks (8.0 or later)
* CLISP (`ros install clisp-head`, build from [here](https://gitlab.com/gnu-clisp/clisp/-/merge_requests/3), or wait until this branch is merged)

## Tests

`(asdf:test-system :trivial-package-local-nicknames)` runs the test suite formerly known as 
[package-local-nicknames-tests](https://github.com/phoe/package-local-nicknames-tests/).
If you need to modify the system to add an extra implementation, then only modify the `DEFPACKAGE`
form to import the proper symbols from your implementation's extension package. Unless you are 
adding (implementation-specific) test functionality, do not modify the tests themselves.

## License

Unlicense / Public domain
