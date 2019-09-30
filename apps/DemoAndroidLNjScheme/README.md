# LNjScheme

This directory contains an app to demo how to use LNjScheme from LN.

LNjScheme is derived from the 1.4 version of
[Jscheme from Peter Norvig](http://norvig.com/jscheme.html).

Jscheme version 1.4, the last version that I released (in April
1998). A mercilessly small, easily modifiable version.

(NB: There is another thing going by the name Jscheme, which was
extented by a community until 2006.  This version grew beyond the
features, complexity and size which make the Pter Norvigs version
interesting as a strating point.)

Jscheme 1.4 however lacks a few features, notably the ability supply
arguments to constructors.  Therefore a derivative was required for
LN.  In accordance with the stated license for Jscheme it got a new
name.

## Changes

1. Baseline: unpacked the sources from `jscheme-source.jar` into
   subdirectory `LNjScheme`.
2. Changed package name to LNjScheme and added Makefile.
3. Refine errors raised from application of Java methods.
4. Pull some code from the community version to support constructors with arguments.
5. Copy glue code from experimental branch and rename identifiers.
