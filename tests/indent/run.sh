#!/bin/sh

COUNT_FAIL=0
COUNT_PASS=0

for i in test???; do
  cd $i
  nvim --headless -u ../base.rc -s test.vim test.hs 2> /dev/null
  diff expected.hs result.hs
  if [ $? -eq 0 ]; then
    COUNT_PASS=$((COUNT_PASS+1))
    echo "$(basename $PWD) succeeded"
    rm result.hs
  else
    COUNT_FAIL=$((COUNT_FAIL+1))
    echo "$(basename $PWD) failed"
  fi
  cd ..
done

echo "===="
echo "PASSED: $COUNT_PASS\tFAILED: $COUNT_FAIL"
