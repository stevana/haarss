#!/bin/sh

grep -R "pgmF htfpp" ../src/Haarss/* | \
  cut -d: -f1 | \
  sed -e 's/\.\.\/src\///' \
      -e 's/.hs//' \
      -e 's/\//\./g' \
      -e 's/^/import \{\-@ HTF_TESTS @\-\} /'
