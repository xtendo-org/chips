echo ">>> Preparing integration test"
stack install > /dev/null 2>&1 || exit 1
docker build -t kinoru/chips integration > /dev/null || exit 1
cp ~/.local/bin/chips ./integration
echo ">>> Test start"
docker run -a stdout -t --name chips-integration-test -v $(pwd $(dirname $0))/integration:/home/user/integration kinoru/chips /usr/bin/bash /home/user/integration/test.sh
rm ./integration/chips
docker stop chips-integration-test > /dev/null
docker rm chips-integration-test > /dev/null
export RESULT=$(cat ./integration/result.txt)
rm ./integration/result.txt
if test "$RESULT" = "success"; then
    echo ">>> Test success"
    exit 0
else
    echo ">>> Test fail"
    exit 1
fi
