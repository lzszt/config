update-pin:
	set -e
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git https://github.com/nixos/nixpkgs.git > nix/.pinned-nixpkgs.json"
.PHONY: update-pin