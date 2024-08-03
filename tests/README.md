Comprehensive Testing
=====================
Each directory has a script called `run.sh`.
Execute it to run the test.

`./binder/` tests the binder code for correctness
`./bindings/` tests the generated binding with a sample implementation.
`./coverage` checks the binder and binding testsuite to ensure 100% code coverage
`./testdata/` is not a test, but stores resources used by other tests.

`./run.sh` runs all tests and fails if any of the tests fails.

Make sure to build the binder in both dev and coverage mode (`BUILD_MODE=<mode>` gprbuild) from the root directory first.
