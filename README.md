
# Introduction

LambdaNative is a cross-platform development environment
written in Scheme, supporting Android, iOS, OS X, Linux
and Windows.

# Getting started

## Required SDKs

The minimum requirement for developing applications is the
presence of a working gcc compiler. In addition,
cross-compilation requires installation of the appropriate
environments:

### Android (mac or linux) 
Install the android SDK and NDK
under `/usr/local` and the API that you want to develop for.

### iOS (mac)
Install the iOS SDK under `/Developer` to develop iOS applications.

### Linux cross-compilation (mac)
Install a linux cross-compiler under `/usr/local` to create
linux binaries.

### Windows cross-compiler (mac or linux)
Install a windows cross-compiler under `/usr/local` to
create windows binaries.

### Windows development environment
If you are developing on a windows machine (not
recommended), install the MSYS development environment.

## Required tools

A number of tools are needed to support the framework.
Please ensure that these are installed on your system:

* `wget` for pulling library code from the net
* `freetype` for rendering vector fonts
* `netpbm` and `ImageMagick` for misc pixmap manipulation
* `ghostscript` for converting vector artwork
* `cmake` for generating XCode projects
* `tgif` for editing vector artwork (optional)
* `fruitstrap` for installing iphone apps (optional)

## SETUP and PROFILE

Use the provided `SETUP.template` and `PROFILE.template` to
create files `SETUP` and `PROFILE` in the framework
directory matching your setup.

## First build

To start using the framework, type:

    ./configure DemoHelloWorld
    make

this will take a while, as the supporting libraries are
downloaded and compiled for the first time. On completion,
you will have a binary for your local host.

On a suitably configured platform, you can now do:
    
    ./configure DemoHelloWorld android
    make

    ./configure DemoHelloWorld iphone
    make

and binaries for the specified platforms will be built and
packaged.

## Directory structure

    apps/                       applications
    libraries/                  supporting libraries
    modules/                    application modules
    modules/config              application configuration
    modules/eventloop           event handling for GUI applications
    modules/ln_core             general supporting functions
    modules/ln_glcore           low level OpenGL wrapper
    modules/ln_glgui            widget based GUI
    modules/ln_audio            cross-platform audio handling
    bootstraps/                 platform launchers
    tools/pngtools              png texture generator
    tools/ttftools              ttf texture generator

