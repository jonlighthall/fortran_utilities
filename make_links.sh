#!/bin/bash

# print source name at start
echo "${TAB}running $BASH_SOURCE..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

TAB="   "

# generate executables before linking
make

# set source and target directories
source_dir=$(dirname "$src_name")/bin
target_dir=$HOME/bin

# check directories
echo -n "source directory ${source_dir}... "
if [ -d "$source_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "target directory ${target_dir}... "
if [ -d $target_dir ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv $target_dir
fi

echo "--------------------------------------"
echo "------ Start Linking Repo Files-------"
echo "--------------------------------------"

# list of files to be linked
ext=.exe
for my_link in cpddiff \
		prsdiff \
		tldiff \
		tsdiff
do
    target=${source_dir}/${my_link}${ext}
    link=${target_dir}/${my_link}

    echo -n "source file ${target}... "
    if [ -e "${target}" ]; then
	echo -n "exists and is "
	if [ -x "${target}" ]; then
	    echo "executable"
	echo -n "${TAB}link ${link}... "
	# first, backup existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
	    echo -n "exists and "
	    if [[ "${target}" -ef ${link} ]]; then
                echo "already points to ${my_link}"
		echo -n "${TAB}"
		ls -lhG --color=auto ${link}
		echo "${TAB}skipping..."
		continue
	    else
		if [ $(diff "${target}" ${link} | wc -c) -eq 0 ]; then
		    echo "have the same contents"
		    continue
		else
		    echo -n "will be backed up..."
		    mv -v ${link} ${link}_$(date +'%Y-%m-%d-t%H%M')
		fi
	    fi
	else
	    echo "does not exist"
	fi
        # then link
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} | sed "s/^/${TAB}/"
        else
            echo -e "not executable"
        fi
    else
	echo "does not exist"
    fi
done
echo "--------------------------------------"
echo "--------- Done Making Links ----------"
echo "--------------------------------------"
# print time at exit
echo -en "$(date +"%R") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    echo "$(sec2elap $SECONDS)"
else
    echo "elapsed time is ${SECONDS} sec"
fi
