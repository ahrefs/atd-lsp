# atd-lsp 

`opam install .` will install a `atdlsp` executable under `$OPAM_SWITCH_PREFIX/bin/atdlsp`.


## Usage
### Neovim 
Neovim has built-in LSP client. You can find a sample config in [init.lua](./init.lua).

### VSCode

There's a VSCode extension that works as an LSP client. [src/](./src/) contains the source code for the VSCode extension. It's published on [VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=tysg.atd-vscode).

## Supported features

- Go to definition
- Document symbols
