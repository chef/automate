#!/usr/bin/env bash
#go to directory
cd ../components/automate-ui
# execute npm test inside automate_ui
exec npm test
#list all the files
exec ls
#copy lcov.info 
#srcdir="coverage/lcov.info"                   
#dstdir="../../Coverage_report/"
source="coverage/lcov.info"
destination="../../Coverage_report/"
#d=$(date +%m%d%y)
if [ -f $source]; then
      cp "$source" "$destination"
fi

