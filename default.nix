{ stdenv

# Native
, gnat
, gprbuild

# Deps
, xmlada
}:

stdenv.mkDerivation {
   pname = "dbus-binding-generator-ada";
   version = "0.0";
   src = ./.;
   
   nativeBuildInputs = [
      gprbuild
      gnat
   ];

   buildInputs = [
      xmlada
   ];

   buildPhase = ''
      runHook preBuild
      gprbuild -j0 -XBUILD_MODE=prod -XLIBRARY_TYPE=relocatable
      runHook postBuild
   '';

   installPhase = ''
      gprinstall -m -p -Pdbus_binding_generator_ada -XBUILD_MODE=prod -XLIBRARY_TYPE=relocatable --prefix=$out --mode=usage --no-project --no-manifest
   '';
}
