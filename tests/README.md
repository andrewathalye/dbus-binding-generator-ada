Comprehensive Testing
=====================
Each directory has a script called `run.sh`.
Execute it to run the test.

`./coverage` checks the testsuite itself to ensure 100% coverage
`./binder/` tests the binder code for correctness
`./bindings/` tests the generated binding with a sample implementation.
`./testdata/` is not a test, but stores resources used by other tests.
