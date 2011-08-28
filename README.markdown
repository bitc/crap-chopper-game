Chopper Game
============

Bit Connor  
bit@mutantlemon.com  
Written in May 2006

This is a simple unfinished game written in Haskell. It has 3D OpenGL graphics
and a chopper(Helicopter) that you can fly around.

It is written in a very "functional" style. There are no IOVars used to
maintain state. Instead, an "update the world" pure function is used to update
the game state:

    step :: ChopperControls -> TimeStep -> GameState -> GameState

Building
--------

The code probably needs to be updated to build with recent GHC and libraries.

Controls
--------

*  w,a,s,d - Fly around
*  c,space - Fly up and down
*  Mouse - Aim and shoot
