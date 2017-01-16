#!/usr/bin/env bash
bold=$(tput bold)
normal=$(tput sgr0)

echo "$bold>>> Preparing integration test$normal"
stack install > /dev/null || exit 1
docker build -t xtendo-org/chips integration > /dev/null || exit 1
cp ~/.local/bin/chips ./integration

echo "$bold>>> Test start$normal"
docker run -a stdout -t --name chips-integration-test -v $(pwd $(dirname $0))/integration:/home/user/integration xtendo-org/chips /usr/bin/bash /home/user/integration/test.sh
rm ./integration/chips
docker stop chips-integration-test > /dev/null
docker rm chips-integration-test > /dev/null

export RESULT=$(cat ./integration/result.txt)
rm ./integration/result.txt
if test "$RESULT" = "success"; then
    echo "$bold>>> Test success$normal"
    exit 0
else
    echo "$bold>>> Test fail$normal"
    exit 1
fi
