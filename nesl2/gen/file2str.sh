#!/bin/sh
#
# wrapper script for file2str program
#

if [ $# != 3 ] ; then
  echo "usage: file2str srcfile module dstfile"
  exit 1
fi

SRCFILE=$1
MODULE=$2
DSTFILE=$3

SRC=./gen/file2str.sml

sml $SRC 2> /dev/null 1>/dev/null <<XXXX
File2Str.doit("$SRCFILE", "$MODULE", "$DSTFILE");
XXXX
