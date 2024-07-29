DBus Binding Generator Ada
==========================
Generates convenient and high-level Ada bindings for arbitrary D\_Bus specifications.

Compile and run `./dbus_binding_generator_ada` to see tool usage.

The tool generates Ada packages corresponding to D\_Bus interfaces and outputs them to stdout.
Use `gnatstub` and `gnatpp` to clean up the generated code as desired.

TODO
----
Allow mixing servers and clients in the same application.
At the moment it is possible but requires manually modifying bindings.
Handle annotations and allow custom type names.

Subdirectories
--------------
`data/`: XML schemas and D\_Bus specifications
`supportlib/`: The support library
`examples/`: Example code to verify that building clients and servers works.
`tests/`: Test scripts to verify coverage and generated servers.

Dev Tools
---------
`./cycle.sh` perform one full build and generation cycle on "$@". The output will go to `examples/generated/`
`./format.sh` format the binding generator code using `gnatpp`

Client Binding Usage
--------------------
See `examples/src/mpris.adb` for an example client application.
This duplicates the functionality of `playerctl -a metadata`.

Server Binding Usage
--------------------
See `examples/src/mpris_server.adb` for an example server application.
You can verify that it works by calling `playerctl -a metadata`

Licence
-------
This code is GPL-licensed, except that the DBus support libraries `D_Bus.Support` and `D_Bus.Extra` are both
GPL with Linking Exception (as per the GNAT source code).
