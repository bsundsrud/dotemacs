# Spacemacs without Spacemacs

Emacs config meant to work for me, personally, while still looking good.  Faster to start up than spacemacs, but server mode and `emacsclient` is still recommended.

## Installation

1. `git clone git@github.com:bsundsrud/dotemacs.git ~/.emacs.d`
2. Start emacs
3. run `M-x all-the-icons-install-fonts RET`
4. restart emacs for pretty icons

## Optional Dependencies

### Rust

This setup uses RLS and rustfmt when in `rust-mode`.  Use the following rustup command to install all the necessary components:

* `rustup component add rls rust-analysis rust-src rustfmt`

### JavaScript

This setup uses [SourceGraph's JS/TS LSP server](https://github.com/sourcegraph/javascript-typescript-langserver) for JS completion in `js2-mode`.  Install it with the following:

* `npm install -g javascript-typescript-langserver`
