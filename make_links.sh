#!/bin/bash -u

# load formatting
fpretty="${HOME}/utils/bash/.bashrc_pretty"
if [ -e "$fpretty" ]; then
    source "$fpretty"
fi

# determine if script is being sourced or executed
if (return 0 2>/dev/null); then
    RUN_TYPE="sourcing"
else
	RUN_TYPE="executing"
	# exit on errors
	set -e
fi
# print source name at start
echo -e "${TAB}${RUN_TYPE} ${PSDIR}$BASH_SOURCE${NORMAL}..."
src_name=$(readlink -f "$BASH_SOURCE")
if [ ! "$BASH_SOURCE" = "$src_name" ]; then
    echo -e "${TAB}${VALID}link${NORMAL} -> $src_name"
fi

# generate executables before linking
make

# set source and target directories
target_dir=$(dirname "$src_name")/bin
link_dir=$HOME/bin

# check directories
echo -n "source directory ${target_dir}... "
if [ -d "$target_dir" ]; then
    echo "exists"
else
    echo -e "${BAD}does not exist${NORMAL}"
    exit 1
fi

echo -n "link directory ${link_dir}... "
if [ -d "$link_dir" ]; then
    echo "exists"
else
    echo "does not exist"
    mkdir -pv "$link_dir"
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

	# check if target exists
	echo -n "target file ${target}... "
    if [ -e "${target}" ]; then
		echo -n "exists and "
		permOK=500
		perm=$(stat -c "%a" "${target}")
		if [[ ${perm} -ge ${permOK} ]]; then
			if [ -x "${target}" ]; then
				echo -n "is"
			else
				echo -n "should be"
			fi
			echo " executable"
			# begin linking...
			echo -n "${TAB}link ${link}... "
			TAB+=${fTAB:='   '}
			# first, check for existing copy
			if [ -L "${link}" ] || [ -f "${link}" ] || [ -d "${link}" ]; then
				echo -n "exists and "
				if [[ "${target}" -ef "${link}" ]]; then
					echo "already points to ${my_link}"
					echo -n "${TAB}"
					ls -lhG --color=auto "${link}"
					echo "${TAB}skipping..."
					TAB=${TAB%$fTAB}
					continue
				else
					# next, delete or backup existing copy
					if [ $(diff -ebwB "${target}" "${link}" 2>&1 | wc -c) -eq 0 ]; then
						echo "has the same contents"
						echo -n "${TAB}deleting... "
						rm -v "${link}"
					else
						if [ -e "${link}" ]; then
                        echo "will be backed up..."
                        mdate=$(date -r "${link}" +'%Y-%m-%d-t%H%M')
                    else
                        echo -n "is a broken link..."
                        mdate=$(stat -c '%y' ${link} | sed 's/\(^[0-9-]*\) \([0-9:]*\)\..*$/\1-t\2/' | sed 's/://g')
                    fi
                    link_copy="${link}_${mdate}"
                    mv -v "${link}" "${link_copy}" | sed "s/^/${TAB}/"
					fi
				fi
			else
				echo "does not exist"
			fi
			# then link
			echo -en "${TAB}${GRH}"
			hline 72
			echo "${TAB}making link... "
			ln -sv "${target}" "${link}" | sed "s/^/${TAB}/"
			echo -ne "${TAB}"
			hline 72
			echo -en "${NORMAL}"
			TAB=${TAB%$fTAB}		else
			echo -e "${BAD}not executable${NORMAL}"
        fi
	else
		echo -e "${BAD}does not exist${NORMAL}"
	fi
done
bar 38 "--------- Done Making Links ----------"
# print time at exit
echo -en "$(date +"%a %b %-d %-l:%M %p %Z") ${BASH_SOURCE##*/} "
if command -v sec2elap &>/dev/null; then
	bash sec2elap $SECONDS
else
	echo "elapsed time is ${SECONDS} sec"
fi
