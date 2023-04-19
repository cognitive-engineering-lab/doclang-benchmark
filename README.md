# Doclang Benchmark

## Setup

Prerequisites: [npm], [pdflatex], and [pandoc].

```
cd scripts
pip3 install -e .
python3 bench.py install
```

## Running the benchmark

```
python3 bench.py all
```

See the `output/` directory for outputs.


## Tasks

* `basic-formatting`: universal document primitives. Paragraphs, inline styles (incl nesting), sections. Tests baseline conciseness.
* `list-map`: mapping over a list, specifically generating bullets from a string array. Tests compile-time computation with data structures.
* `reactive-slider`: updating the page when user moves a slider. Tests model-view separation and reactivity w/ run-time computation.

Brainstorm for other tasks:
* Nested documents: custom components which contain nontrivial document fragments
* Complex data structures: pass around a fancy object that can't be stringifed as an HTML attribute
* Macros: custom functions that are sprinkled around the text. Or maybe just definitions/references?
* Errors: what kinds of undefined behavior is permitted by the complex runtime-enabled frameworks?

General thoughts:
* Goal is to measure the *languages* more than their *component libraries*. How can we distinguish between those two?
* Are character length / token length going to be useful measures? Might be interesting to count the *number of unique language mechanisms*, eg for langs that have a gajillion special cases.


## Languages

* `tex`: [pdflatex] implementation of LaTeX.
* `md`: [CommonMark] Markdown.
* `mdx`: [MDX], a Markdown variant with support for JSX syntax. 
* `jsx`: [React JSX] syntax and runtime.
* `nota`: [Nota] document language.
* `lpub`: [Living Papers] document language.
* `scrbl`: [Scribble] documentation tool for Racket.

Other candidates for inclusion:
* [Markdoc](https://markdoc.dev/): seems to only support compile-time computation.
* [Pandoc][pandoc]: used by Living Papers and Quarto, so probably just pick one of those instead.
* [Quarto](https://quarto.org/): very similar to Living Papers, unclear if we need another comparison.
* [Pollen](https://docs.racket-lang.org/pollen/): relatively niche, mostly a contrast to Scribble.
* [reStructuredText](https://docutils.sourceforge.io/rst.html): more customizable than Markdown but seems to be static-only.

[npm]: https://npmjs.org/
[pdflatex]: https://www.math.rug.nl/~trentelman/jacob/pdflatex/pdflatex.html
[CommonMark]: https://commonmark.org/
[MDX]: https://mdxjs.com/
[React JSX]: https://react.dev/
[Nota]: https://nota-lang.org/
[Living Papers]: https://github.com/uwdata/living-papers/
[Scribble]: https://docs.racket-lang.org/scribble/index.html
[pandoc]: https://pandoc.org/