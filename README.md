[chips]

A plugin manager for [fish].

## Features

- (supposed to be) Fast. Compiled to the native machine code. Parallel installation/upgrade of plugins.
- Minimally invasive to your `config.fish`: Adding one line is sufficient.
- Minimum overhead to the fish shell itself. The amount of fish script to be loaded for chips to work is extremely little.
- Minimum installation cost: No dependency except `git` and `curl`. chips is a single statically-compiled binary executable file.
- Uninstallation is easy: Remove chips, `~/.config/chips`, and the line in `config.fish`, and you're clean as if you never installed chips at all.

## Installation

Current version: **chips 1.1.2** (2017-01-16)

### GNU/Linux (x64)

Assuming `~/.local/bin` is in your `$PATH`:

```fish
curl -Lo ~/.local/bin/chips --create-dirs \
    https://github.com/xtendo-org/chips/releases/download/1.1.2/chips_gnulinux \
    ; and chmod +x ~/.local/bin/chips
```

### OS X (or macOS)

Assuming `~/.local/bin` is in your `$PATH`:

```fish
curl -Lo ~/.local/bin/chips --create-dirs \
    https://github.com/xtendo-org/chips/releases/download/1.1.2/chips_osx \
    ; and chmod +x ~/.local/bin/chips
```

### Installation the hard way: Build from source

Use [Stack].

```fish
git clone --depth=1 https://github.com/xtendo-org/chips
cd chips
stack install
```

## Usage

1. Run `chips`. This will create `~/.config/chips/plugin.yaml` with the default template.
1. Edit `plugin.yaml` to include your desired plugins. For example, consider [fish-sensible] or [shellder].
1. Run `chips; exec fish`.

After this, any time you make changes to `plugin.yaml` or want to update plugins, run `chips` again.

## Supported features

- Sourcing `init.fish` of plugins
- Installing themes: Plugins that contain `fish_prompt.fish` or `fish_right_prompt.fish`
- `functions` provided by plugins

## To do

- Completely purging unused plugins
- Files under `completions`

## Officially recommended plugins

- [fish-sensible]
- [shellder]
- [cgitc]

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

## FAQ

### Why not fisherman?

chips is not written by someone who abuses DMCA takedown to bully other free software projects. In fact, OMF getting shot down was the first motivation to write chips.

[chips]: https://en.wikipedia.org/wiki/Fish_and_chips
[fish]: https://fishshell.com/
[Stack]: http://haskellstack.org/
[fish-sensible]: https://github.com/simnalamburt/fish-sensible
[shellder]: https://github.com/simnalamburt/shellder
[cgitc]: https://github.com/simnalamburt/cgitc
