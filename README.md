# Haskell Elm Architecture

A Haskell program to mimic the Elm Architecture

The `main` function runs a state loop where the user can enter a `message` via the command line. The `model` (a single `integer`) will update whilst performing any effects needed. It uses the `StateT` [monad transformer](http://book.realworldhaskell.org/read/monad-transformers.html) to combine stateful computations and IO together.

### Installing and Running

The project is built using [`stack`](https://docs.haskellstack.org/en/stable/README/) (install guides [here](https://www.youtube.com/watch?v=sRonIB8ZStw) and [here](https://docs.haskellstack.org/en/stable/README/)).

install and build the project by running:

```sh
> stack build
```

To run the program: (`tea` is "the-elm-architecture")

```sh
> stack exec tea
```

This will prompt you to enter a message. The program will recognise:

+ `addOne`
+ `addTwo`
+ `minusOne`
+ `minusTwo`
+ `sayHello`
+ `currentDir`

These can be in caps or lowercase

If the program doesn't recognise a message it will return `NoOp`
