#!/bin/bash
# exit on errors
set -e

# set tab
:${TAB:=''}

# load formatting
fpretty=${HOME}/utils/bash/.bashrc_pretty
if [ -e $fpretty ]; then
    source $fpretty
fi

# print source name at start
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
    RUN_TYPE="executing"
fi
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f $BASH_SOURCE)
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# generate executables before linking
make

# set target and link directories
target_dir=$(dirname "$src_name")/bin
link_dir=$HOME/bin

# check directories
echo -n "target directory ${target_dir}... "
if [ -d "$target_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "link directory ${link_dir}... "
if [ -d $link_dir ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv $link_dir
fi

bar 38 "------ Start Linking Repo Files-------"

# list of files to be linked
ext=.exe
for my_link in cpddiff \
		prsdiff \
		tldiff \
		tsdiff
do
    # define target (source)
    target=${target_dir}/${my_link}${ext}
    # define link (destination)
    link=${link_dir}/${my_link}

    echo -n "target file ${target}... "
    if [ -e "${target}" ]; then
	echo "exists "

	# next, check file permissions
	if true; then
	    echo -n "${TAB}${target##*/} requires specific permissions: "
	    permOK=700
	    echo "${permOK}"
	    TAB+=${fTAB:='   '}
	    echo -n "${TAB}checking permissions... "
	    perm=$(stat -c "%a" ${target})
	    echo ${perm}
	    if [[ ${perm} -gt ${permOK}  ]]; then
		echo -n "${TAB}changing permissions to ${permOK}... "
		chmod u+x ${target}
		RETVAL=$?
		if [ $RETVAL -eq 0 ]; then
		    echo -e "${GOOD}OK${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
		else
		    echo -e "${BAD}FAIL${NORMAL} ${gray}RETVAL=$RETVAL${NORMAL}"
		fi
	    else
		echo -e "${TAB}permissions ${GOOD}OK${NORMAL}"
	    fi
	    TAB=${TAB%$fTAB}
	fi

	echo -n "${TAB}link $link... "
	TAB+=${fTAB:='   '}
	# first, backup existing copy
	if [ -L ${link} ] || [ -f ${link} ] || [ -d ${link} ]; then
	    echo -n "exists and "
	    if [[ "${target}" -ef ${link} ]]; then
                echo "already points to ${my_link}"
		echo -n "${TAB}"
		ls -lhG --color=auto ${link}
		echo "${TAB}skipping..."
		TAB=${TAB%$fTAB}
		continue
	    else
		if [ $(diff -ebwB "${target}" ${link} | wc -c) -eq 0 ]; then
		    echo "have the same contents"
		    echo -n "${TAB}deleting... "
		    rm -v ${link}
		else
		    echo -n "will be backed up..."
		    mv -v ${link} ${link}_$(date -r ${link} +'%Y-%m-%d-t%H%M')
		fi
	    fi
	else
	    echo "does not exist"
	fi
        # then link
	echo -en "${TAB}${GRH}";hline 72;
	echo "${TAB}making link... "
	ln -sv "${target}" ${link} | sed "s/^/${TAB}/"
	echo -ne "${TAB}";hline 72;echo -en "${NORMAL}"
	TAB=${TAB%$fTAB}
    else
        echo -e "${BAD}does not exist${NORMAL}"
    fi
done
bar 38 "--------- Done Making Links ----------"

# print time at exit
echo -en "\n$(date +"%a %b %-d %I:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
    sec2elap ${SECONDS}
else
    echo "elapsed time is ${SECONDS} sec"
fi
