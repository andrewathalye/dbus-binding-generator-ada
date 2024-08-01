Bindings Tests
==============

This directory contains tests for the client, server, and mixed bindings
generated by the binder.

`./run.sh` runs the test suite.

It uses `dbus-send` to test error conditions that the binder would normally
not expose, and then three Ada programs to test client, server, and mixed bindings.