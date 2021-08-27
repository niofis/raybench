{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
        rustc
	cargo
	nodejs-16_x
	go
	nim
	zig
	dotnet-sdk_5
	lua
	luajit
    ];
}
