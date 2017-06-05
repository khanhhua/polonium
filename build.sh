#!/bin/sh

echo "Removing _build/default..."
rm -rf _build/default

echo "Building with profile 'default'..."
rebar3 release
