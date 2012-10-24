turing - A library implementing a Turing machine.

Installation
------------

    cabal install turing

Usage
-----

For example usage see examples/MoveRight.hs in which a very simple Turing
machine is defined. You can test this example in GHCi as following.

    > :l MoveRight.hs
    > interactive tm1 $ fromList [ I, O, I, I, O, I ]
    > OOIOIIOIOO
    >   ^
    >   S
    > (press return)
    > OOOOIIOIOO
    >    ^
    >    i
    > (press return)
    > OOOIIIOIOO
    >     ^
    >     o
    > (press return)
    > OOOI0IOIOO
    >      ^
    >      i
    > (press return)
    > OOOI0IOIOO
    >       ^
    >       i
    > (press return)
    > OOOI0IIIOO
    >        ^
    >        o
    > (press return)
    > OOOI0II0OOO
    >         ^
    >         i
    > (press return)
    > OOOI0II0IOOO
    >          ^
    >          o
    > (press return)
    > OOOI0II0IOOOO
    >           ^
    >           o
    > (press Ctrl-c)

