{ pkgs, ... }:

{
  home.packages = [
    pkgs.cargo
    pkgs.deno
    pkgs.difftastic
    pkgs.neovim
    pkgs.nodejs
    pkgs.rust-analyzer
    pkgs.rustc
    pkgs.rustfmt
  ];

  programs.claude-code.enable = true;

  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "c000";
        email = "c000@users.noreply.github.com";
      };
    };
  };
}
