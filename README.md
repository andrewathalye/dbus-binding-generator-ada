DBus Binding Generator Ada
==========================
Generates convenient and high-level Ada bindings for arbitrary D\_Bus specifications.

Compile and run `./dbus_binding_generator_ada` to see tool usage.

The tool generates Ada packages corresponding to D\_Bus interfaces and outputs them to stdout.
Use `gnatstub` and `gnatpp` to clean up the generated code as desired.

TODO
----
Add support for Deprecated and EmitsChangedSignal

Let the program work even without `data/introspect.xsd`

Subdirectories
--------------
`data/`: The XSD schema for specification validation
`supportlib/`: A support library for Object-Oriented D\_Bus
`examples/`: Example code for the client and server bindings
`tests/`: Test scripts for the binder and generated bindings.

Dev Tools
---------
`./format.sh` format the binding generator code using `gnatpp`

Supported Annotations
---------------------
| Annotation                                         | Values                            | Meaning                                                                      |
| :------------------------------------------------- | :-------------------------------- | :--------------------------------------------------------------------------- |
| `org.freedesktop.DBus.Deprecated`                  | `true, false`                     | Emits `pragma Obsolescent` for the entity in question.                       |
| `org.freedesktop.DBus.Method.NoReply`              | `true, false`                     | Suppresses producing or waiting for a reply for a method call.               |
| `org.freedesktop.DBus.Property.EmitsChangedSignal` | `true, invalidates, const, false` | Controls whether `PropertiesChanged` is emitted when a property is modified. |

Licence
-------
This code is GPL-licensed, except that the DBus support library `D\_Bus.Support` is licensed the same as D\_Bus/Ada (GPL with GNAT Linking Exception).
