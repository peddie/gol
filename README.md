Conway's Game of Life
=================

Work in progress.  Currently it has no real documentation or
command-line arguments, it defaults to the 3D version, and it's
bizarrely and unwisely organized.  The gloss navigation isn't very
good for this application, and I haven't figured out the transparency
or any colors.

To get the code:
----------

    git clone git://github.com/peddie/gol.git

To build the program:
---------

    cd gol
    cabal build

To run:
---------

    dist/build/GameOfLife/GameOfLife 20 +RTS -N

which uses a 20x20x20 grid.  Grids larger than about 25 on a side tend
to really slow down the not-gloss output -- I'm working on it.  The
standard `simulate` keybindings from `not-gloss` are in effect, so you
can tilt with the left mouse button, pan with the right mouse button
and zoom in and out with `e` and `q` respectively.
