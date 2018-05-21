#!/bin/sh

for i in test???; do
  cd $i
  nvim --headless -s test.vim test.hs 2> /dev/null
  diff expected.hs result.hs
  if [ $? -eq 0 ]; then
    echo "$(basename $PWD) succeded"
    rm result.hs
  else
    echo "$(basename $PWD) failed"
  fi
  cd ..
done
