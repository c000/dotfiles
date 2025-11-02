{ pkgs, ... }:

{
  home.packages = [
    pkgs.cargo
    pkgs.deno
    pkgs.difftastic
    pkgs.neovim
    pkgs.rust-analyzer
    pkgs.rustc
    pkgs.rustfmt
  ];
}
