alias mutt="mutt -F ~/.config/mutt/mutt.conf"
alias copy="xclip -sel clip"
alias paste="xclip -o -sel clip"

source <(antibody init)

antibody bundle < ~/.zsh_plugins
