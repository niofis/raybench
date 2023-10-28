# with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {});
# with (import <nixpkgs> {});
with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/08ef0f28e3a41424b92ba1d203de64257a9fca6a.tar.gz") {});
mkShell {
    nativeBuildInputs = with buildPackages; [
		ack
		rustc
		cargo
		nodejs-20_x
		go
		nim
		zig
		dotnet-sdk_5
		lua
		luajit
		haxe
		swift
		openjdk
		scala
		sbcl
		wabt
		wasmer
		zig
    ];
}
