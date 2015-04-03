#!/bin/sh

############################################################
# Shell snippet for creating a package tarball while
#  testing and updating ccdr code
############################################################

# NOTE: --exclude needs to come BEFORE the rest of the arguments, otherwise this fails
cd /Users/Zigmund-2/rgithub/
tar --exclude='./.git*' --exclude='./.Rproj*' --exclude='*.so' --exclude='*.o' --exclude='./beta*' -cvzf ./ccdr/beta/ccdr.tar.gz ./ccdr
