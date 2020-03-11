# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
	
### Added

### Changed
	
### Fixed

## [1.3.1] - 2020-03-11

Bugfix

### Added

### Changed
	
### Fixed
-  Needed to enable cmake testing.
	
## [1.3.0] - 2020-03-11

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
	
## [1.2.0] - 2018-12-18

Releasing as open source.

  - including NOSA COPYRIGHT (soon to be changed to Apache)
  - including LICENSE
  - adding forgotten compiler-specific cmake files (oops)
	
## [1.1.0] - 2018-08-08

Mists of time ...
	
## [1.0.0] - 2018-08-03
	
Mists of time ...
