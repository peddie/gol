Conway's Game of Life
=================

Work in progress.

To get the code:
----------

    git clone git://github.com/peddie/gol.git

To build the program:
---------

    cd gol
    cabal build

To run:
---------

    dist/build/GameOfLife/GameOfLife 100 +RTS -N4

which uses a 100x100 grid.  Grids larger than about 100 on a side tend
to really slow down the not-gloss output -- I'm working on it.  The
standard `simulate` keybindings from `not-gloss` are in effect, so you
can tilt with the left mouse button, pan with the right mouse button
and zoom in and out with `e` and `q` respectively.

No documentation yet; sorry.
