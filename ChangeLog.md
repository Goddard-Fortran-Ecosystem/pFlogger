# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

### Changed

- Eliminated use of submodules; gFTL must be preinstalled
- Using FindPackage to find pFUnit (not required)
- Updated to pFUnit 4.0
- Changed naming conventions for modules (dropping the _mod suffix)
- Introduced dependency on gFTL-shared
  . eliminates redundant code
  . makes container types are consistent across projects
- Now uses yafyaml for YAML processing (much more complete YAML subset)
	
### Fixed

- Corrected Python code generator scripts for component import/export specs.
- Add directories to `.gitignore` for building with `mepo`
	
## [1.0.0] - 2019-02-07

Mists of time ...
