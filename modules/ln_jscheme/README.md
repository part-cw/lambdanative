# LNjScheme

This directory contains an app to demo how to use LNjScheme from LN.

LNjScheme allows to call any Java/Android method from
lambdanative/gambit without additional JNI code.  Either directly or
within the UI thread (dispatched asynchronously via `runOnUiThread`).

## Build

call `make -f Makefile` in this directory to create `android_jars/LNjScheme.jar`.

# History

LNjScheme is derived from the 1.4 version of
[Jscheme from Peter Norvig](http://norvig.com/jscheme.html).

Jscheme version 1.4, the last version that I released (in April
1998). A mercilessly small, easily modifiable version.

(NB: There is another thing going by the name Jscheme, which was
extented by a community until 2006.  This version grew beyond the
features, complexity and size which make the Peter Norvigs version
interesting as a strating point.)

Jscheme 1.4 however lacks a few features, notably the ability supply
arguments to constructors.  Therefore a derivative was required for
LN.  In accordance with the stated license for Jscheme it got a new
name.

## Changes

1. Baseline: unpacked the sources from `jscheme-source.jar` into
   subdirectory `LNjScheme`.
2. Changed package name to LNjScheme and added Makefile.
3. Refined errors raised from application of Java methods.
4. Pulled some code from the community version to support constructors with arguments.
5. Copied glue code from experimental branch and rename identifiers.

# Issues

## Numbers

jScheme uses `lang.java.Double` for all numbers.  This does not play
nice with native Jave APIs.  TBD: Teach it either about fixnums or
conversion; or both.

## Missing

Really missing is the ability to access arbitrary field values (as
apposed to calling methods) of any class.

From
https://stackoverflow.com/questions/13400075/reflection-generic-get-field-value

    import java.lang.reflect.Field;

     Field chap = c.getDeclaredField("chapters");
            out.format(fmt, "before", "chapters", book.chapters);
            chap.setLong(book, 12);
            out.format(fmt, "after", "chapters", chap.getLong(book));

    Field[] fields = c.getDeclaredFields();
            for (Field classField : fields)
            {
                result.add(classField);
            }
