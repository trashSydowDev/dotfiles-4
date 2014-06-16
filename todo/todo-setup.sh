#!env sh
##
# Sets-up and configures todo.txt
##

if ! type "todo.sh" > /dev/null; then
  echo "todo.txt not installed.\ninstalling."
  mkdir -p $HOME/.bin
  curl -L https://github.com/downloads/ginatrapani/todo.txt-cli/todo.txt_cli-2.9.zip > $HOME/todo-cli.zip
  unzip $HOME/todo-cli.zip -d $HOME
  mv $HOME/todo.txt_cli-2.9/todo.sh $HOME/.bin
  rm -rf $HOME/todo-cli.zip $HOME/todo.txt_cli-2.9
  chmod +x $HOME/.bin/todo.sh
fi

if [ ! -d "$HOME/todo" ]; then
  mkdir $HOME/todo
  for i in `ls dotfiles/todo`; do
    if [[ $i == "todo-setup.sh" ]]; then continue; fi
    cp $i $HOME/todo/$i
  done
fi

# from https://github.com/fujimogn/dotfiles/blob/master/todo.txt/todo.txt.zsh
function t() {
  if [ $# -eq 0 ]; then
    todo.sh ls
  else
    todo.sh $*
  fi
}

#compdef _todo.sh t

source $HOME/todo/todo.cfg

alias td="todo.sh"
alias tn="td ls +next"
alias ta="td add"
