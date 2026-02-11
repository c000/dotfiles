{ pkgs, ... }:

{
  home.packages = [
    pkgs.cargo
    pkgs.claude-code
    pkgs.deno
    pkgs.difftastic
    pkgs.neovim
    pkgs.nodejs
    pkgs.rust-analyzer
    pkgs.rustc
    pkgs.rustfmt
  ];
}
