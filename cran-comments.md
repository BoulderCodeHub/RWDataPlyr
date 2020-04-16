# RWDataPlyr v0.6.4 Submission 

Submitted to work with upcoming dplyr v1.0.0.

## Test environments
* local Windows 10 Enterprise, R 3.6.3
* ubuntu 16.04 (GitHub Actions), R 3.5, and 3.6.3
* windows (GitHub Actions), R 3.5, R 3.6.3, and R-devel
* MacOS-latest (GitHub Actions), R 3.5, R 3.6.3, and R-devel
* R-hub - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* R-hub - Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub - Fedora Linux, R-devel, clang, gfortran
* R-hub - Debian Linux, R-devel, GCC ASAN/UBSAN
* Win-builder - R 3.6.3 and R-devel

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTE on all except:

1 error on MacOS-latest with R 3.5:
ERROR: dependency ‘tibble’ is not available for package ‘RWDataPlyr’

This is not an issue with RWDataPlyr; it is either an issue with GitHub Actions 
or tibble.

1 Note on R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit:
checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    'examples_x64' 'tests_i386' 'tests_x64'
    'RWDataPlyr-Ex_i386.Rout' 'RWDataPlyr-Ex_x64.Rout' 'examples_i386'

This appears to be an issue on R-hub and not in RWDataPlyr.

## Downstream dependencies
There are currently no downstream dependencies for this package.
