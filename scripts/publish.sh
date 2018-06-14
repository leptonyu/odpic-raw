#!/bin/sh
PRG="$0"
# Need this for relative symlinks.
while [ -h "$PRG" ] ; do
    ls=`ls -ld "$PRG"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "$link" : '/.*' > /dev/null; then
        PRG="$link"
    else
        PRG=`dirname "$PRG"`"/$link"
    fi
done

ROOT=`dirname "$PRG"`
ROOT=`cd "$ROOT/..";pwd`

cd "$ROOT"

if [ -z "$HACKAGE_USER" -o -z "$HACKAGE_PASS" ]; then
  echo "HACKAGE_USER or HACKAGE_PASS not set"
  exit 1
fi

update=`git diff-index --name-status HEAD | wc -l`

if [ "$update" -gt 0 ]; then
  echo "git has uncommitted modifications"
  exit 1
fi

# version=`grep '^version' odpic-raw.cabal | awk '{print $2}'`

# sed -i.bak "s|hackage-v[0-9][0-9]*\(.[0-9][0-9]*\)*-orange.svg|hackage-v$version-orange.svg|" README.md
# sed -i.bak "s|\(odpic-raw-\)[0-9][0-9]*\(.[0-9][0-9]*\)*|\1$version|" README.md
# rm -f README.md.bak

# git add README.md
# git commit -m "publish v$version"

# git tag v$version
# git push origin master
# git push --tag

stack haddock

pkg=.

stack sdist $pkg && stack upload $pkg
hup docboth -u $HACKAGE_USER -p $HACKAGE_PASS

