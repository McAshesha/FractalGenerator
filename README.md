# FractalGenerator

A small Haskell CLI that draws classic mathematical fractals straight in your
terminal. Built on top of `ansi-terminal`, `parallel` and `hspec`.

Currently supported:

* Mandelbrot Set
* Julia Set (classic seed `c = -0.7 + 0.27015i`)
* Sierpinski Triangle (bit-AND chaos game)

## Getting Started

Requires GHC 9.6+ and Stack 2.15+.

```bash
stack build
stack run
```

Follow the on-screen menus to pick a fractal. Maximize the terminal window and
shrink the font for the best results.

## Tests

```bash
stack test
```

## License

GPL — see the LICENSE file for the full text.
