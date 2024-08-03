{
   inputs.nix-ada.url = "github:andrewathalye/nix-ada/v1.5";
   #inputs.nix-ada.url = "git+file:///home/andrew/src/ada/nix-ada/";

   outputs = { self, nix-ada }:
   let
      nix-ada_s = nix-ada.packages.x86_64-linux;
   in
   with nix-ada_s;
   {
      devShells.x86_64-linux.default = import nix/shell.nix { nix-ada = nix-ada_s; };
      packages.x86_64-linux.default = pkgs.callPackage nix/default.nix { inherit (nix-ada_s) dbus-ada; };
   };
}
