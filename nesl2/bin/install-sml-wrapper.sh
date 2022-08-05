#!/bin/sh
#
# COPYRIGHT (c) 2006 The SML/NJ Fellowship.
#
# bin/install-sml-wrapper.sh.  Generated from install-sml-wrapper_sh.in by configure.
#
# a general-purpose script for installing executable wrappers for SML/NJ
# programs.
#
#	install-sml-wrapper.sh <program-name> <install-dir>

#INSTALL="/opt/local/bin/ginstall -c"
INSTALL="/usr/bin/install -c"
INSTALL_DATA="${INSTALL} -m 644"
HEAP_SUFFIX=$(sml @SMLsuffix)

if test $# -ne 2 ; then
  echo "usage: install-sml-wrapper.sh <program-name> <install-dir>"
  exit 1
fi

SRC=$1
TARGET=`basename $SRC`
HEAP_IMAGE=$SRC.$HEAP_SUFFIX
INSTALL_DIR=$2
INSTALL_HEAP_DIR=$INSTALL_DIR/.heap
INSTALL_HEAP_IMAGE=$INSTALL_HEAP_DIR/$TARGET.$HEAP_SUFFIX

if test ! -f $HEAP_IMAGE ; then
  echo "heap image $HEAP_IMAGE not found"
  exit 1
fi

# create the wrapper script
#
cat > $TARGET <<XXXX
#!/bin/sh
#
exec /usr/local/bin/sml @SMLcmdname=\$0 @SMLload=$INSTALL_HEAP_IMAGE \$@
XXXX

#install the script and heap image
#
if test ! -d $INSTALL_DIR ; then
  mkdir -p $INSTALL_DIR || exit 1
fi
$INSTALL $TARGET $INSTALL_DIR/$TARGET || exit 1
if test ! -d $INSTALL_HEAP_DIR ; then
  mkdir -p $INSTALL_HEAP_DIR || exit 1
fi
$INSTALL_DATA $HEAP_IMAGE $INSTALL_HEAP_IMAGE || exit 1

# remove the local copy of the script
rm -f $TARGET

exit 0

