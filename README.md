# [chips]

A plugin manager for [fish].

## Advantages

Boost your friendly interactive shell with plugins.

- Fast: **Parallel installation/upgrade** of plugins.
- Minimally invasive to your `config.fish`: Adding **one** line is sufficient
- Zero overhead to the fish shell itself: **Only plugins will be sourced**, chips itself doesn't require any fish script to be loaded at all to work
- Zero dependency and minimum installation cost: chips is a single statically-compiled binary executable file

[Not an April Fools' joke](https://github.com/kinoru/chips/commit/0b87850ab0a658391b643f0ecc37f748dac89010).

## Usage

1. Run `chips`. This will create `~/.config/chips/plugin.yaml` with the default template.
1. Edit `plugin.yaml` to include your desired plugins. For example, consider [fish-sensible] or [shellder].
1. Run `chips` again.

Any time you make changes to `plugin.yaml` or you want to update plugins, run `chips` again.

## Supported features

- Sourcing `init.fish` of plugins
- Installing themes: Plugins that contain `fish_prompt.fish` or `fish_right_prompt.fish`

## Coming soon

- Completely purging unused plugins
- Files under `functions` and `completions`

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

[chips]: https://en.wikipedia.org/wiki/Fish_and_chips
[fish]: https://fishshell.com/
[Stack]: http://haskellstack.org/
[fish-sensible]: https://github.com/simnalamburt/fish-sensible
[shellder]: https://github.com/simnalamburt/shellder
