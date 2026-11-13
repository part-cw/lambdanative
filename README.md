[![fedora-build](https://github.com/part-cw/lambdanative/actions/workflows/fedora-build.yml/badge.svg)](https://github.com/part-cw/lambdanative/actions/workflows/fedora-build.yml) [![macos-build](https://github.com/part-cw/lambdanative/actions/workflows/macos-build.yml/badge.svg)](https://github.com/part-cw/lambdanative/actions/workflows/macos-build.yml) [![windows-build](https://github.com/part-cw/lambdanative/actions/workflows/windows-build.yml/badge.svg)](https://github.com/part-cw/lambdanative/actions/workflows/windows-build.yml)

# LambdaNative is in need of new maintainers. Please see https://github.com/part-cw/lambdanative/issues/475 for details

## Introduction
LambdaNative is an open-source (BSD licensed) cross-platform development environment written in Scheme ([Gambit-C](https://github.com/feeley/gambit)), supporting Android, iOS, BlackBerry 10, OS X, Linux, Windows, OpenBSD, NetBSD, FreeBSD and OpenWrt.

If you use the LambdaNative framework for your own work, please reference this [ICFP paper](https://github.com/part-cw/lambdanative/blob/master/docs/lambdanative_icfp13.pdf?raw=true).
A slideshow describing the LambdaNative framework is available [here](https://github.com/part-cw/lambdanative/blob/master/docs/LambdaNative.pdf?raw=true).

The repository contains a number of [demonstration programs](apps), inlcuding the games [uSquish](apps/USquish) and [LineDrop](apps/LineDrop), and a [Calculator](apps/Calculator).

## Documentation
Instructions for getting started and API documentation can be found in the wiki:
[ **LambdaNative Documentation and Support** ](../../wiki/Home)

## Android builds
Android builds require JDK 17 or newer and Android SDK Platform 35. The Gradle Wrapper downloads the pinned Gradle version automatically. See the [Android build requirements](targets/android/README.md) for SDK/NDK details.

## Feedback and Contributions
Please help contribute bug reports, code fixes (ideally in the form of pull requests), corrections and missing information about API calls using the project's [issue tracker](https://github.com/part-cw/lambdanative/issues?direction=desc&page=1&sort=created&state=open). You might find solutions to past problems in the [closed issues](https://github.com/part-cw/lambdanative/issues?q=is%3Aissue+is%3Aclosed). We also recently added a beta [GitHub Discussions](https://github.com/part-cw/lambdanative/discussions) feature for general discussions. Finally, to contribute documentation and examples, please modify the [wiki](../../wiki/Home) directly. - Your contributions are much appreciated!

## Acknowledgements
Contributions by John Adjei ([@AgoeZero](https://github.com/AgoeZero)) and Benson Muite ([@bkmgit](https://github.com/bkmgit)) from 2024-26 were generously supported by [NLnet](https://nlnet.nl/) through the [NGI Mobifree Fund](https://nlnet.nl/mobifree), a fund established with financial support from the European Commission's [Next Generation Internet](https://ngi.eu/) program.
