Scran
=====

Introduction
------------

Scran is an entity-component framework written in more or less portable R5RS Scheme.  It is based on [paldepind](https://github.com/paldepind)'s [Kran](https://github.com/paldepind/Kran) and it joins [cljan](https://github.com/VincentToups/cljan) as a port of that library into a Lisp.  Scran, unlike cljan, is written for performance on mobile devices, and hence is, in some ways, simpler than Kran and much simpler than Cljan, which is purely functional and monadic.

But they all share essentially similar interfaces.

Scran is independent from any game engine or rendering setup and could be used for any type of game.  

Note: Scran does not have the notion of system groups, although Kran does.  These are easily expressed in terms of lists of systems.  See the example below.

Usage:
------

As in Kran/Cljan one always uses Scran in the following way:

1.   Declare/create components.  These are pieces of data which entities in the game may have (eg: health, magic, position, sprite)
2.   Declare/create systems.  Systems are defined by a list of components.  Any entity which has those components is automatically added to the system, and entities which lose components will be removed from the system.  Systems create behaviors.  For instance, an entity may have a position and a gravity component.  The (gravity position) system then defines the code to cause it to accelerate downward each frame.
3.   Start the game.  Presumably, during the progress of the game, entities are created, components are added and removed from them, and they may be destroyed.

WARNING: you must create all components before creating systems and all systems before creating entities.  Otherwise, Scran's behavior is undefined, and probably it will break.

Example
-------


    (load "modules/scran/scran.scm")

    ;; define two components
    (define c-growth-stage (component! (lambda (e v) v)))
    (define c-growth-rate (component! (lambda (e v) v)))

    ;; define a system using these components
    (define growing-things 
      (system! (list c-growth-stage c-growth-rate)
       every: 
       (lambda (e stage rate) 
        (entity-component-set! e c-growth-stage (+ stage    rate)))))

    ;; create some entities
    (let loop ((i 0))
      (if (< i 10)
       (begin (entity! 
        (list c-growth-stage 0)
        (list c-growth-rate (random-integer 10)))
        (loop (+ i 1)))
       #t))

    ;; define a utility

    (define (get-growth-stages)
     (system-map-entities 
      (lambda (e stage rate)
      stage)
     growing-things))

    ;; show pre-execution stages
    (display (get-growth-stages)) (newline) 
    ;; (0 0 0 0 0 0 0 0 0 0)

    ;; execute our system a few times
    (let loop ((i 0))
     (if (< i 10)
      (begin (system-execute growing-things)
      (loop (+ i 1)))
     #t))

    ;; show post-execution stages
    (display (get-growth-stages)) (newline) 
    ;; eg: (80 20 70 80 30 0 70 10 20 70)


System Groups
-------------

Kran is oriented towards groups of systems.  Scran exposes systems directly, if opaquely.  You can easily create system groups like this:

    (define make-system-group list)
    (define (execute-group group)
     (map system-execute group))

Macros
------

In accord with the gambit-c style, we separate out macros for using scran into a separate `macros.scm` file.  Include this directly into your source if you wish to use the macro interface.


API Documentation
-----------------

Scran is one file.  The API functions are sectioned off from the support functions and are at the top of the file.  They are fairly well documented.

I know it works with gambit-c/lambdanative.  Hence, I expect it to work with Scheme Spheres.  It might need some tweeking to work on other Schemes.

