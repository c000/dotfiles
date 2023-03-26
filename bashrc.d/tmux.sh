tmux() {
  if [[ $# -eq 0 ]]; then
    command tmux new-session -A -s main "$@"
  else
    command tmux "$@"
  fi
}
