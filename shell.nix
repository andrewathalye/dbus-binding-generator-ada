{ nix-ada }:

nix-ada.pkgs.mkShell {
   nativeBuildInputs = [
      nix-ada.gprbuild  
      nix-ada.gnat
      nix-ada.libadalang-tools
      nix-ada.ada-language-server
      nix-ada.pkgs.nodejs
      nix-ada.pkgs.gdb
      nix-ada.pkgs.lcov
   ];
      
   buildInputs = [
      nix-ada.pkgs.xmlada
      nix-ada.dbus-ada
   ];

   shellHook = ''
      export LIBRARY_TYPE=relocatable
   '';
}
