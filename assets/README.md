# Assets

This folder contains scripts and outputs for generating real screenshots and animated GIFs from the Tuition examples, following the style used in Charmbracelet READMEs.

## Generating GIFs with VHS

We use [VHS](https://github.com/charmbracelet/vhs) to script and record terminal sessions into GIFs.

1) Install VHS (on macOS, Linux, or Windows): see the VHS README for install instructions.
2) From the project root, run VHS on any of the tapes in `assets/vhs/`.

Examples:

```
vhs assets/vhs/spinner.tape
vhs assets/vhs/progress.tape
vhs assets/vhs/textinput.tape
vhs assets/vhs/table.tape
vhs assets/vhs/markdown.tape
```

The scripts will produce GIFs under `assets/gifs/`. The README references these outputs.

Note: ensure `sbcl` is available and this repo is on your ASDF source registry, or use the `asdf:load-system` call as scripted.

