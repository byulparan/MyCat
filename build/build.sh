#!/bin/bash

appName="MyCat"

# If it was installed in a different location, correct it to the proper one.
cl_nextstep="~/quicklisp/local-projects/lib/cl-nextstep/libcl-nextstep.dylib"

################
#  App Bundle  #
################

if [ -d $appName.app ]; then rm -rf $appName.app; fi

mkdir $appName.app
mkdir $appName.app/Contents
mkdir $appName.app/Contents/MacOS
mkdir $appName.app/Contents/Resources
mkdir $appName.app/Contents/Frameworks

cp Info.plist $appName.app/Contents
cp -R ../assets/* $appName.app/Contents/Resources


###############
#  Framework  #
###############

#
if [ ! -f  $cl_nextstep ];then
  echo "You should be install and build cl-nextstep"
  exit 1
fi

cp $cl_nextstep $appName.app/Contents/Frameworks
install_name_tool -id @executable_path/../Frameworks/libcl-nextstep.dylib $appName.app/Contents/Frameworks/libcl-nextstep.dylib


###########
#  build  #
###########

sbcl --load build.lisp --eval "(save-lisp-and-die \"$appName.app/Contents/MacOS/sbcl.core\" :toplevel #'cl-user::build-toplevel-function)"
cp /usr/local/bin/sbcl $appName.app/Contents/MacOS/$appName



