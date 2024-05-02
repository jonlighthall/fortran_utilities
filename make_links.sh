#!/bin/bash -u

# get starting time in nanoseconds
start_time=$(date +%s%N)

# load formatting
fpretty=${HOME}/config/.bashrc_pretty
if [ -e "$fpretty" ]; then
    source "$fpretty"
    set_traps
    # reset tab
    rtab
fi

# determine if script is being sourced or executed
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
    # exit on errors
    set -e
fi
print_source

# generate executables before linking
cbar "Start Compiling"
make
cbar "Done Compiling"

# set target and link directories
target_dir="${src_dir_phys}/bin"
link_dir=$HOME/bin

# check directories
check_target ${target_dir}
do_make_dir ${link_dir}

cbar "Start Making Links"
# list of files to be linked
ext=''
for my_link in cpddiff \
    prsdiff \
    tldiff \
    tsdiff; do

    # define target (source)
    target=${target_dir}/${my_link}${ext}

    # define link (destination)
    link=${link_dir}/${my_link}

    # create link
    decho "linking $target to $link..."
    do_link_exe "$target" "$link"
done
cbar "Done Making Links"
