# Haskell Elm Architecture

A Haskell program to mimic the Elm Architecture

The `main` function runs a state loop where the user can enter a `message` via the command line. The `model` (a single `integer`) will update whilst performing any effects needed. It uses the `StateT` [monad transformer](http://book.realworldhaskell.org/read/monad-transformers.html) to combine stateful computations and IO.

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

You can then enter a message via the command line. The program will recognise:

+ `add 1`
+ `add2 `
+ `hello`
+ `dir`

If the program will print the current state if it doesn't recognise the input
