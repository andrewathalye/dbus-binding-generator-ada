DBus Binding Generator Ada
==========================
Generates convenient and high-level Ada bindings for arbitrary D\_Bus specifications.

The tool generates Ada packages corresponding to D\_Bus interfaces and outputs them to stdout.
Use `gnatstub` and `gnatpp` to clean up the generated code as desired.

Run the tool with no arguments to see its help page.

Building
--------
Build using Nix: `nix build` or GPRBuild: `gprbuild` (from the project root)

Installing is currently only supported via Nix flakes. Dependencies are canonically listed
in `nix/default.nix`. For a manual install, be sure to set "PREFIX" to the install prefix.

Development
-----------
`nix develop`

This provides several debugging and development tools as well.

Limitations
-----------
On POSIX systems, the number of file descriptors that a process can have open at once
is typically limited. Exceeding this limit will raise Constraint\_Error upon attempting
to receive a call from a D\_Bus method / signal that takes file descriptors.

Subdirectories
--------------
```
supportlib/ : A support library for Object-Oriented D\_Bus
src/        : Source code for the binder
share/      : Introspect schema location
examples/   : Example code for the client and server bindings
specs/      : Some sample D_Bus specifications from real applications.
tests/      : Test scripts for the binder and generated bindings.
nix/, gpr/  : Build system implementation details
```

Dev Tools
---------
`./format.sh` format the binding generator code using `gnatpp`

Supported Annotations
---------------------
| Annotation                                         | Values                            | Meaning                                                                         |
| :------------------------------------------------- | :-------------------------------- | :------------------------------------------------------------------------------ |
| `org.freedesktop.DBus.Deprecated`                  | `true, false`                     | Emits `pragma Obsolescent` for the entity in question.                          |
| `org.freedesktop.DBus.Method.NoReply`              | `true, false`                     | Suppresses producing or waiting for a reply for a method call\*                 |
| `org.freedesktop.DBus.Property.EmitsChangedSignal` | `true, invalidates, const, false` | Controls whether `PropertiesChanged` is emitted when a property is modified\*\* |

\* This is not actually implemented directly by the binder, but supported nonetheless.
\*\* If a property with `invalidates` is modified, `PropertiesChanged` signals will not contain the new value. `const` and `false` are treated identically.

Licence
-------
This code is GPL-licensed, except that the DBus support library `D\_Bus.Support` is licensed the same as D\_Bus/Ada (GPL with GNAT Linking Exception).
