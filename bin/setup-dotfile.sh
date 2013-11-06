#!/bin/bash
# setup-dotfiles
#
# Author: yamadapc <github.com/yamadapc> 2013
# Simple shell script to create the necessary symlinks to my home folder, from
# the dotfiles folder - nothing intelligent going on here

LINKED_FILES=("jshintrc" "slate" "tmux.conf" "vimrc" "zshrc" "vimperatorrc")

function main() {
  for i in ${LINKED_FILES[@]} ; do
    echo -s "$HOME/dotfiles/$i" "$HOME/.$i"
  done
  setup_vim
}

function setup_vim() {
  echo $HOME/.vim
  echo $HOME/.vim/backup
  echo $HOME/.vim/tmp
  echo $HOME/.vim/bundle
  echo $HOME/.vim/undo
  cd $HOME/.vim/bundle
  git clone https://github.com/gmarik/vundle $HOME/.vim/bundle
}

main
