#!/bin/bash

for i in test???; do
  pushd $i > /dev/null
  nvim --headless -s test.vim test.hs 2> /dev/null
  diff expected.hs result.hs
  if [ $? -eq 0 ]; then
    echo "$(basename $PWD) succeded"
    rm result.hs
  else
    echo "$(basename $PWD) failed"
    exit 1
  fi
  popd > /dev/null
done
