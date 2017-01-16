# Change Log

## 1.1.1 - 2016-11-09

### Fixed

- The chips executable after self-update will now have the `+x` permission.
- Executing chips with command line arguments will not cause shell exit anymore.
- Now works with the version of fish included in Ubuntu 14.04 LTS.

## 1.1.0 - 2016-10-02

### Added

- Automatic self update
- Support for plugins with "functions"
- Integration testing with Docker
- Theme uninstall

### Fixed

- Failure when `$HOME` and `/tmp` were not in the same disk.
- Failure when config.fish is missing
- Hang due to GitHub request burst (added request intervals)
- Misbehavior when chips is run offline

## 1.0.0 - 2016-04-01

- Initial release.
