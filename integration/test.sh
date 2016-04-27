function _die {
    echo 'fail' > ./result.txt
    exit 1
}
function _assert {
    set -e
    export TMP=$1
    shift
    "$@" && echo "[ OK ] "$TMP || (echo "[Fail] "$TMP; _die)
    set +e
}

cd /home/user/integration
./chips > ~/chips_initial_run.txt
_assert "Run message without configuration" diff chips_initial_run.txt ~/chips_initial_run.txt
_assert "plugin.yaml" diff default.plugin.yaml ~/.config/chips/plugin.yaml

cp plugin.yaml ~/.config/chips/plugin.yaml
./chips > ~/chips_config_run.txt
head -n 2 ~/chips_config_run.txt > ~/chips_config_run_head.txt
tail -n 3 ~/chips_config_run.txt > ~/chips_config_run_tail.txt

_assert "Run message with configuration (1)" diff install.txt ~/chips_config_run_head.txt
_assert "Run message with configuration (2)" grep -q ^Installing\ shellder...\$ ~/chips_config_run.txt
_assert "Run message with configuration (3)" grep -q ^Installing\ fish-sensible...\$ ~/chips_config_run.txt
_assert "Run message with configuration (4)" grep -q ^Installing\ friendly-engine...\$ ~/chips_config_run.txt
_assert "Run message with configuration (5)" diff chips_second_run_tail.txt ~/chips_config_run_tail.txt

_assert "build.fish" diff build.fish ~/.config/chips/build.fish
_assert "fish_prompt.fish" diff fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish

echo "success" > ./result.txt
