DBus Binding Generator Ada
==========================
Generates high-level, convenient Ada bindings given a DBus XML specification.

Compile and run `./dbus_binding_generator_ada` to see tool usage.

Test XML specifications can be found in `data/`, test code can be found in `test/`

Client Binding Usage
--------------------
```ada
with Ada.Strings.Unbounded;
with Interfaces;

with org_freedesktop_portal_FileChooser;
with org_freedesktop_portal_Print;

with D_Bus.Support;

procedure Main is
    type OFP_Subset is new D_Bus.Support.Root_Object
        and org_freedesktop_portal_FileChooser
        and org_freedesktop_portal_Print;

    OFP_Desktop : OFP_Subset;
begin
    OFP_Desktop.Create (Ada.Strings.Unbounded.To_Unbounded_String ("/org/freedesktop/portal/desktop"));
    --  Assign a D_Bus path to this Ada proxy object

    OFP_Desktop.Set_Destination ("org.freedesktop.portal.Destkop");
    --  Assign a destination so libdbus knows where to send messages

    --  Now call any and all methods on /org/freedesktop/portal/desktop as if they were Ada subprograms.
    declare
        Value : Interfaces.Unsigned_32;
    begin
        OFP_Desktop.MethodName (parameter => Value);

        --  OFP_Subset is a subset of all the interfaces that
        --  OFP_Desktop implements. You probably wouldn’t actually
        --  need to access every interface at once, but even if you did
        --  it would still be possible (and easy) using these bindings!
        OFP_Desktop.OpenFile (<>);
        OFP_Desktop.Print (<>);
    end;

    --  Signals work as follows:
    OFP_Desktop.Register_SignalName;
    --  Start listening for this signal
    declare
        Result : Ada.Strings.Unbounded.Unbounded_String;
    begin
        OFP_Desktop.Await_SignalName (Result);
        --  Block until the signal is emitted
    end;
    OFP_Desktop.Unregister_SignalName;
    --  Stop listening for this signal

    --  Properties are easy too:
    Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (OFP_Desktop.PropertyName));
    OFP_Desktop.Set_PropertyName (Ada.Strings.Unbounded.To_Unbounded_String ("Test Value"));
    --  * note that properties only work if the interface `org.freedesktop.DBus.Properties` is
    --  implemented by the object in question.

    OFP_Desktop.Destroy;
    --  Clean up everything
end Main;
```

Server Binding Usage
--------------------
TODO

Licence
-------
    This code is GPL-licensed, except that the DBus support libraries `D_Bus.Support` and `D_Bus.Extra` are both
GPL with Linking Exception (as per the GNAT source code). In this way generated code should be usable in proprietary
applications, provided that the applications themselves acknowledge that they link to `libdbusada` and `libdbusada-support`.
