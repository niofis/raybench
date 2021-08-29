with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {});
mkShell {
    nativeBuildInputs = with buildPackages; [
        rustc
	cargo
	nodejs-16_x
	go
	nim
	zig
	dotnet-sdk_5
	lua
	luajit
	haxe
	swift
    ];
}
