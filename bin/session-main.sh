#!/bin/bash
# session-main
# Author: yamadapc <github.com/yamadapc> 2013
# Simple shell script to set-up my main tmux session.
MAIN_NAME="main"
red="\e[0;31m"
blue="\e[1;34m"
green="\e[1;32m"
cyan="\e[1;36m"
NC="\e[0m" # no color
# if unable to attach
if [ ! `tmux attach -t $MAIN_NAME` ] ; then
    echo -e "${red}Main session doesn't exist.${NC}"

    echo -e "${blue}>> ${cyan}Creating main session...${NC}" ; sleep 1
    tmux new-session -d -s $MAIN_NAME

    echo -e "${blue}>> ${cyan}Creating default windows...${NC}"

    # default windows to start
    windows=("m" "edit" "read" "compile/run" "sys" "music")
    let "i=0"
    for w in ${windows[@]} ; do
        WINDOW_CMD="tmux neww -k -t $MAIN_NAME:$i -n $w"

        if [ $w == "sys" ]; then
            eval "$WINDOW_CMD \"top -o cpu\""
        elif [ $w == "music" ]; then
            eval "tmux neww -k -t $MAIN_NAME:9 -n $w \"cmus\""
        else
            eval $WINDOW_CMD
        fi
        echo -e "\t${green}$w created${NC}"; sleep 0.1
        let "i+=1"
    done
    echo -e "${blue}>> ${cyan}Attaching to main session...${NC}"; sleep 1
    tmux select-window -t 0
    tmux attach -t $MAIN_NAME
fi
