{ stdenv

# Native
, gnat
, gprbuild

# Deps
, xmlada
, dbus-ada
}:

stdenv.mkDerivation rec {
   pname = "dbus-binding-generator-ada";
   version = "1.0.1";
   src = ./..;

   outputs = ["out" "lib" "dev"];

   dbus-binding-generator-ada-schema = ../share/introspect.xsd;
   
   nativeBuildInputs = [
      gprbuild
      gnat
   ];

   buildInputs = [
      xmlada
      dbus-ada
   ];

   buildPhase = ''
      runHook preBuild
      export BUILD_MODE=prod
      export LIBRARY_TYPE=relocatable
      export INTROSPECT_SCHEMA=${dbus-binding-generator-ada-schema}

      gprbuild -j0
      gprbuild -j0 -Psupportlib/dbusada_support.gpr
      runHook postBuild
   '';

   installPhase = ''
      runHook preInstall
      export BUILD_MODE=prod
      export LIBRARY_TYPE=relocatable

      gprinstall -p -Pdbus_binding_generator_ada --prefix=$out --mode=usage --no-project --no-manifest
      gprinstall -m -p -Psupportlib/dbusada_support.gpr --prefix=$dev --lib-subdir=$lib/lib --mode=dev --no-manifest

      runHook postInstall
   '';
}
