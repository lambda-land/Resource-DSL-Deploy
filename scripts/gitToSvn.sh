#!/bin/bash

GIT=".git*"
DS=".DS_Store"
DESTFOLDER=""
NEWFOLDER=""
SRCFOLDER=""
second_to_last=${@:-2:1}
for last; do true; done
last_arg=$last
DRYRUN=false
PRNUSAGE=false
EXCLUDE="--exclude=$GIT --exclude=$DS"
RARGS=" -ar"

removeGit () {
    if [ -z "$1" ]
    then
        echo "Parameter one is zero length!"
    else
        find "$1" -maxdepth 2 -type d -name $GIT -exec ls -ld {} \;
    fi
}

validateDir () {
    if [[ ! -d "$1" ]];
    then
        echo "$1 is not a directory or does not exist!"
        exit 1
    fi
}

usage () {
    echo "Usage: $0 [-dh] [-n new_folder] gitSourceRepo svnDestRepo"
    echo "Moves files in gitSourceRepo to svnDestRepo smartly not overwriting "
    echo "files that are newer than files that are being moved"
    echo
    echo "Optional Arguments are as follows:"
    echo "  -d                    Perform a dry-run of the move"
    echo "  -h                    Display this help information"
    echo "  -n=NEW_FOLDER         Perform the copy to Dest/NEW_FOLDER"
}

# parse the options passed in
while getopts ":dahn:" opt ; do
    case $opt in
        d) RARGS+="vn"
           DRYRUN=true;;
        h) PRNUSAGE=true ;;
        n) NEWFOLDER=$OPTARG ;;
        \?) echo "Invalid Option: -$OPTARG" >&2
            exit 1
            ;;
    esac
done

# grab out the mandatory arguments
for arg in "$@"
do
    if [[ $arg != -* ]] && [[ -z "$SRCFOLDER" ]] && [[ "$arg" != "$NEWFOLDER" ]] ;
    then
        SRCFOLDER=$arg
    elif [[ $arg != -* ]] && [[ -n "$SRCFOLDER" ]] && [[ "arg" != "$NEWFOLDER" ]];
    then
        DESTFOLDER=$arg
    fi
done


if [[ $PRNUSAGE == true ]] ;
   then
       usage
       exit 0
fi


# now process the arguments
if [[ -n "$SRCFOLDER" ]] && [[ -n "$DESTFOLDER" ]];
then
    # check inputs to be existant
    validateDir "$SRCFOLDER"
    validateDir "$DESTFOLDER"

    # set the destination folder to have the new folder name
    DESTFOLDER=$DESTFOLDER"/"$NEWFOLDER

    if [[ $DRYRUN == true ]] ;
       then
           echo "Running a dry run"
           echo "Source Dir: $SRCFOLDER"
           echo "Destination Dir: $DESTFOLDER"
           echo -e "These files would be transferred!\n"
    fi

    # perform the rsync call
    rsync $RARGS $EXCLUDE $SRCFOLDER"/" $DESTFOLDER

    if [[ $DRYRUN == false ]] ;
       then
           echo "All Done!"
           echo "Use: "
           echo "    svn status                                                          --^ to see svn status"
           echo "    svn add *                                                           --^ to add all files not in a directory"
           echo "    svn add * --force                                                   --^ to add it all"
           echo "    svn revert . -R                                                     --^ to revert tracked files"
           echo "    svn ci -m \"your commit message here\"                                --^ to commit what you've added"
           echo "    or "
           echo "    svn ci -m \"commit message here\" --username uname --password pass    --^ to commit what you've added"
           echo "    svn update                                                          --^ to push to the remote repo"
    fi
else
    usage
fi
