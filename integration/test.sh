red='\e[31m'
green='\e[32m'
normal=$(tput sgr0)

function _die {
    echo 'fail' > ./result.txt
    exit 1
}
function _assert {
    set -e
    export TMP=$1
    shift
    "$@" && echo -e "$green[ OK ]$normal "$TMP || (echo "$red[Fail]$normal "$TMP; _die)
    set +e
}

cd /home/user/integration

echo "Scenario: Run chips without configuration"
./chips > ~/chips_initial_run.txt
_assert "Run message" diff chips_initial_run.txt ~/chips_initial_run.txt
_assert "plugin.yaml" diff default.plugin.yaml ~/.config/chips/plugin.yaml

echo "Scenario: Run chips with configuration"
cp plugin.yaml ~/.config/chips/plugin.yaml
./chips > ~/chips_config_run.txt
head -n 2 ~/chips_config_run.txt > ~/chips_config_run_head.txt
tail -n 3 ~/chips_config_run.txt > ~/chips_config_run_tail.txt

_assert "Run message (1) header" diff install.txt ~/chips_config_run_head.txt
_assert "Run message (2) Installing shellder..." grep -q ^Installing\ shellder...\$ ~/chips_config_run.txt
_assert "Run message (3) Installing fish-sensible..." grep -q ^Installing\ fish-sensible...\$ ~/chips_config_run.txt
_assert "Run message (4) Installing friendly-engine..." grep -q ^Installing\ friendly-engine...\$ ~/chips_config_run.txt
_assert "Run message (5) Installed shellder." grep -q ^Installed\ shellder.\$ ~/chips_config_run.txt
_assert "Run message (6) Installed fish-sensible." grep -q ^Installed\ fish-sensible.\$ ~/chips_config_run.txt
_assert "Run message (7) Installed friendly-engine." grep -q ^Installed\ friendly-engine.\$ ~/chips_config_run.txt
_assert "Run message (8) footer" diff chips_second_run_tail.txt ~/chips_config_run_tail.txt

_assert "build.fish" diff build.fish ~/.config/chips/build.fish
_assert "config.fish" diff config.fish ~/.config/fish/config.fish
_assert "fish_prompt.fish" diff fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish

echo "Scenario: Uninstall plugins"
cp removed.yaml ~/.config/chips/plugin.yaml
./chips > ~/chips_uninstall_run.txt
_assert "No plugin in build.fish" diff removed.build.fish ~/.config/chips/build.fish
_assert "fish_prompt.fish is removed" test ! -e ~/.config/fish/functions/fish_prompt.fish

echo "success" > ./result.txt
