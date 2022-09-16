#!/bin/bash

set -euo pipefail

usage () {
    echo "$0 major|minor|patch"
    exit
}

err() {
    echo "${1:-}"
    exit 1
}

git remote -v | grep -q "git@github.com" || \
    err "git is redonly"
(git push && git status) | grep "Your branch is up to date with 'origin/main'." || \
    err "git is dirty"
rebar3 hex user whoami || \
    err "no hex user. use 'rebar3 hex user auth'"

APPSRC="$(find "$PWD/src" -name "*.app.src")"
[ -z "$APPSRC" ] && usage

SIZE="${1:-patch}"

OVSN=$(grep vsn "$APPSRC" | cut -f2 -d"\"")
MAJOR="$(echo "$OVSN" | cut -f1 -d".")"
MINOR="$(echo "$OVSN" | cut -f2 -d".")"
PATCH="$(echo "$OVSN" | cut -f3 -d".")"

if [ "$SIZE" == "major" ]; then
    NVSN="$((MAJOR + 1)).0.0"
elif [ "$SIZE" == "minor" ]; then
    NVSN="$MAJOR.$((MINOR + 1)).0"
elif [ "$SIZE" == "patch" ]; then
    NVSN="$MAJOR.$MINOR.$((PATCH + 1))"
else
    usage
fi

echo "$APPSRC $OVSN->$NVSN"

sed "s/$OVSN/$NVSN/" < "$APPSRC" > $$ && mv $$ "$APPSRC"
git add "$APPSRC"
git commit -m"v$NVSN"
git tag -a -m"$NVSN" "$NVSN"
git push \
    && git push --tags \
    && rebar3 eunit \
    && rebar3 edoc \
    && rebar3 hex publish \
    && rebar3 hex docs
