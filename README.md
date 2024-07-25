DBus Binding Generator Ada
==========================
Generates high-level, convenient Ada bindings given a DBus XML specification.

Compile and run `./dbus_binding_generator_ada` to see tool usage.

Test XML specifications can be found in `data/`, test code can be found in `test/`

Client Binding Usage
--------------------
See `test/src/mpris_simple.adb` for an extremely simple application.
This duplicates the functionality of `playerctl metadata`.

Server Binding Usage
--------------------
TODO

Licence
-------
    This code is GPL-licensed, except that the DBus support libraries `D_Bus.Support` and `D_Bus.Extra` are both
GPL with Linking Exception (as per the GNAT source code). In this way generated code should be usable in proprietary
applications, provided that the applications themselves acknowledge that they link to `libdbusada` and `libdbusada-support`.
