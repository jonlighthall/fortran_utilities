#!/bin/bash
echo $BASH_SOURCE

make

bin_usr=$HOME/bin
mkdir -pv ${bin_usr}

# list files to be linked in bin
BASH_DIR="$( cd "$( dirname "$0" )" && pwd )"
bin_local=${BASH_DIR}/bin
ext=.exe
for prog in cpddiff \
	    prsdiff \
	    tldiff \
	    tsdiff
do

    link=${bin_usr}/$prog  
    target=${bin_local}/${prog}${ext}
    if [ ! -f $link ]; then
	if [ -f $target ]; then
	    ln -svf $target $link
	else
	    echo "$target not found"
	fi
    else
#	echo -e "$link already exists"
	ls -lhG --color=auto $link
    fi
done
