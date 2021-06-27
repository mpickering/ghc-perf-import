# for testing with nixos-shell
{ pkgs, ... }: {
  boot.kernelPackages = pkgs.linuxPackages_latest;
  imports = [./nixos.nix];
}
