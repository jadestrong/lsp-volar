# lsp-volar
Language support for Vue3

# Installation

## Doom Emacs

in `packages.el`

``` emacs-lisp
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
```

in `config.el`

``` emacs-lisp
(use-package! lsp-volar)
```

## straight.el

``` emacs-lisp
(straight-use-package
 '(lsp-volar :type git :host github :repo "merrickluo/lsp-volar"))
```

# Language Server

1. use `npm/yarn` , `npm install -g @volar/server`

# Customization

## lsp-volar

### `lsp-volar-take-over-mode`
It is enabled by default. If you want disable it, add `(setq lsp-volar-take-over-mode nil)` to your config file. [What is Take Over Mode?](https://github.com/johnsoncodehk/volar/discussions/471 "What is Take Over Mode?") 

# Reference

1. [Tutorial: nvim-lspconfig - how to set up multiple language servers](https://github.com/johnsoncodehk/volar/discussions/606 "Tutorial: nvim-lspconfig - how to set up multiple language servers")  
2. [I have multiple language servers registered for language FOO. Which one will be used when opening a project?](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project "I have multiple language servers registered for language FOO. Which one will be used when opening a project?") 
3. [ I have multiple language servers for language FOO and I want to select the server per project, what can I do?](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-for-language-foo-and-i-want-to-select-the-server-per-project-what-can-i-do " I have multiple language servers for language FOO and I want to select the server per project, what can I do?") 
4. [multiple servers](https://github.com/johnsoncodehk/volar/discussions/393#discussioncomment-1213736 "multiple servers") 
