# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.5.0] - 2021-03-15

### Added

- Pervasive return codes for all procedures with potentially failing branch.
  Added as optional arguments for backward compatibility.
 
### Changed
	
- Introduced CMake namespace.   Upstream projects should now specify dependency
  as PFLOGGER::pflogger

## [1.4.5] - 2020-08-24

### Changed
	
- Fixed one more test that was missed in previous update. (Was using -f filter
  to focus  on some failing tests.  Blinded to thore tests.)


## [1.4.4] - 2020-08-23

### Changed

- Recent addition of `basic_config()` (see previous release)
  introduced an ambiguity for memory management.  The underlying
  design from Python is based upon garbage collection and reference
  counting.  To solve this problem there the user can now either
  specify a HandlerVector or a HandlerPtrVector (but not both) If the
  former, pFlogger will copy the handlers into its internal storage.
  If the latter, pFlogger will only retain a pointer reference to the
  user actual argument.  In that case, the user is repsonsible for
  maintaining the handlers until pFlogger is finalized and ensuring
  that any locks are freed.

- The argument to `Logger::add_handle()`  must now have the TARGET
  attribute.  pFlogger will only maintain a pointer to that target.
  This is to be consistent with the above strategy.

### Fixed

- Implemented workaround for ifort 18.0.5 that became necessary after
  the changes above.  Only impacted unit tests.

## [1.4.3] - 2020-08-07

### Fixed

- Some flavors of MPI complained at finalize because the MPI memory
  windows were not freed.  Fixing required nontrivial changes because
  handlers (and thus locks and windows) were unintentionally
  duplicated rather than shared among logger objects.

## [1.4.2] - 2020-05-20

### Changed

- Modified name of Pair type in gFTL maps.  These are not used within
  pFlogger outside of their host modules, but it is more consistent with
  the latest gFTL-shared and may help anyone that tries to port with XLF.
  

## [1.4.1] - 2020-05-01

### Added
- Added free() methods to various classes to ensure that MPI resources
  are deleted at the end of the run.  Without this, some MPI flavors
  will report an error on MPI_Finalize().    To use:

       call logging%free

## [1.4.0] - 2020-04-17

### Added
  - Added basic_config() method for LoggerManager analogous to that
    of Python's logger.
  - Also added overload of get_logger() with no name argument which
    returns the root logger.  (Again as per Python.)
  - Added example/basic_config/basic_config.F90 (requires MPI)

### Fixed
  - minor bug in default fmt_ for MpiFormatter
    "rank" should have been "mpi_rank".
  
## [1.3.6] - 2020-04-16

### Fixed
  - Trivial change to allow building with older compiler (ifort 17.0.4)

## [1.3.5] - 2020-04-13

### Fixed

  - Mostly workarounds  for gFortran
  - Also corrections for Cmake for some configurations
  
## [1.3.4] - 2020-04-08

### Added
  - Added a "last resort" handler for when no handlers are found.
    (ala Python's logger)
### Fixed
  - A problem with link-time dependency injection for mock MPI was
    fixed with generator expressions.  Previously the fix for tests broke
    the examples
    

## [1.3.3] - 2020-04-06

### Changed
  - Must use true/false instead of .true./.false. for YAML
	
### Fixed
  - Fixed CMake test for support of MPI_ALLOC_MEM
  - Improved Cmake handling of mock to allow tests to compile
    without linking to MPI
  - Added missing init() in Test_MpiLock tests


## [1.3.2] - 2020-03-16

## Fixed

- Cmake installation was using the wrong macro
- allow longer lines (GFortran)

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
