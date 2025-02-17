# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.16.1] - 2025-02-06

### Fixed

- Reverted use of `c_bool` in `MpiLock.F90`. Was causing issues with some compiler/MPI combinations.

## [1.16.0] - 2025-02-03

### Added

- LLVMFlang compiler support
  - NOTE: Issues between LLVMFlang, MPI, and linktime mocking means for now the tests are not built with LLVMFlang

### Changed

- Modified CMake logic to build entirely separate library (pflogger-with-mock) to support testing with mocks. Previous cheat to link pflogger against real MPI for runs but mock MPI for tests did not work with LLVM.   Seems to be due to some encryption type protection on module info.
- Update CI to have `gfortran-10` and `gfortran-11` only on `ubuntu-22.04`
- Update CI NVIDIA to NVHPC 24.7
- Add Flang to CI

## [1.15.0] - 2024-05-17

### Changed

- Change use of `spread` in `MockMPI.F90` initialization to `reshape` to avoid NVHPC issue

## [1.14.0] - 2024-03-26

### Fixed

- Workaround additional polymorphic assignment bug in gfortran 13.2 (in build_locks)

### Changed

- Added `-quiet` flag for NAG Fortran

## [1.13.2] - 2024-03-13

### Fixed

- Another fix for MockMpi layer.  With the workaround for NAG in previous release, GFortran 13 detects some inconsistencies that are now resolved.

## [1.13.1] - 2024-03-07

### Fixed

- Fixed problem encountered due to recent changes in NAG and/or OpenMPI broke the kludge that supports use of a mock MPI layer for testing locks within pFlogger.

## [1.13.0] - 2024-03-03

### Added

- Fujitsu compiler support

### Changed

- Updated dependency on yaFyaml to 1.2.0 as a workaround to gfortran
  13.2 bug with polymorphic assignment.

## [1.12.0] - 2024-01-25

### Changed

- Add new `ENABLE_MPI` option to allow disabling MPI support (#106). By default, MPI is enabled to maintain backward compatibility.


## [1.11.0] - 2023-11-29

### Fixed

- Workarounds for MPICH C_LOC missing interface (https://github.com/pmodels/mpich/issues/6691)
- Add `-check nouninit` for Intel LLVM to work around [`ifx` bug](https://github.com/HPC-Bugs/reproducers/tree/main/compiler/Fortran/ifx/allocatable).

## [1.10.0] - 2023-04-17

### Added

- Added `IntelLLVM.cmake` file as a copy of `Intel.cmake` to support the LLVM Intel compiler frontends

### Changed

- Updated required version of gFTL to v1.10.0
- Updated required version of gFTL-shared to v1.6.0
- Updated required version of yaFyaml to v1.1.0

## [1.9.5] - 2023-04-13

### Changed

- Added Intel to GitHub Actions

## [1.9.4] - 2023-04-12

### Fixed

 - Formatter logic to get simulation time was broken if the default date format is used.

## [1.9.3] - 2023-01-25

### Fixed

- Package was incorrectly assuming that all extant compilers support
  128 bit reals.  Now a check is performed to optionally include
  support for 128 bit reals.
- Fix `Parser()` declaration for Intel 2021.7

## [1.9.2] - 2023-01-23

### Fixed

- Fixes for GNU Make builds

## [1.9.1] - 2022-05-31

### Changed

- Minimum version of yaFyaml is now v1.0.1; contains workarounds for NAG 7.1 (7110)
- Updated GitHub Actions
  - OSs
    - Remove macos-10.15
    - Add ubuntu-22.04 and macos-12
  - Compilers
   - Removed gfortran-8
   - Added gfortran-11
   - Added gfortran-12 (for ubuntu-22.04)

### Fixed

- Workarounds in tests for NAG 7.1 (7110)

## [1.9.0] - 2022-05-08

### Fixed

- Introduced workaround for gFortran when combining strings with unlimited polymorphic entities.  Users may be able to avoid using a `String()` wrapper for args to logging commands now.
- Cleaned up some stray debugging prints
- Fixed misspelling of SUCCESS


## [1.8.0] - 2022-04-08

### Changed

- Updated to new version (1.0-beta7) of yaFyaml which involves new interfaces.

## [1.7.0] - 2022-03-24

### Added

- Add `NVHPC.cmake` and `PGI.cmake` files for NVHPC support

### Fixed

 - Implemented workaround for gfortran 11.2 that was breaking on
   a surprisingly simple call.

### Changed

 - upgraded to use new gFTL2 interfaces for containers.

## [1.6.1] - 2021-11-17

### Changed

- now under the Apache 2.0 license


## [1.6.0] - 2021-09-28


### Changed

- updated to use yaFyaml 1.0 (new interfaces in yafyaml, not pflogger)

### Fixed

- Ability to embed pFlogger alongside the other GFE libraries in a superproject.

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
