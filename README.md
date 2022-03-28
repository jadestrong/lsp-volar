# lsp-volar
Language support for Vue3

**This package has been merged into [lsp-mode](https://github.com/emacs-lsp/lsp-mode), so you can use lsp-mode directly.**
**There will only be some experimental updates here. Once stable, they will be submitted to lsp-mode.**

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

[First, place the following bootstrap code in your init-file:](https://github.com/raxod502/straight.el#getting-started)

``` emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

Then install `lsp-volar` package:
``` emacs-lisp
(straight-use-package
 '(lsp-volar :type git :host github :repo "jadestrong/lsp-volar"))
```

Last use it:

``` emacs-lisp
(use-package lsp-volar
  :straight t)
```

# Hacks

If you prompt `\u0000` *lsp-warn* when performing autocomplete in `Vue template`, you need to use `advice-add` to `override` the following function `lsp--create-filter-function` to filter the special characters. see this issue https://github.com/johnsoncodehk/volar/issues/1118 .

In doom-emacs, you can place this code in your `~/.doom.d/config.el` ,otherwise, you need to use `advice-add` to replace `defadvice!` .

``` emacs-lisp
(defadvice! +lsp--create-filter-function (workspace)
  :override #'lsp--create-filter-function
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (if (s-blank? leftovers)
                      input
                    (concat leftovers input)))

      (let (messages)
        (while (not (s-blank? chunk))
          (if (not body-length)
              ;; Read headers
              (if-let ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                  ;; We've got all the headers, handle them all at once:
                  (setf body-length (lsp--get-body-length
                                     (mapcar #'lsp--parse-header
                                             (split-string
                                              (substring-no-properties chunk
                                                                       (or (string-match-p "Content-Length" chunk)
                                                                           (error "Unable to find Content-Length header."))
                                                                       body-sep-pos)
                                              "\r\n")))
                        body-received 0
                        leftovers nil
                        chunk (substring-no-properties chunk (+ body-sep-pos 4)))

                ;; Haven't found the end of the headers yet. Save everything
                ;; for when the next chunk arrives and await further input.
                (setf leftovers chunk
                      chunk nil))
            (let* ((chunk-length (string-bytes chunk))
                   (left-to-receive (- body-length body-received))
                   (this-body (if (< left-to-receive chunk-length)
                                  (prog1 (substring-no-properties chunk 0 left-to-receive)
                                    (setf chunk (substring-no-properties chunk left-to-receive)))
                                (prog1 chunk
                                  (setf chunk nil))))
                   (body-bytes (string-bytes this-body)))
              (push this-body body)
              (setf body-received (+ body-received body-bytes))
              (when (>= chunk-length left-to-receive)
                (condition-case err
                    (with-temp-buffer
                      (apply #'insert
                             (nreverse
                              (prog1 body
                                (setf leftovers nil
                                      body-length nil
                                      body-received nil
                                      body nil))))
                      (decode-coding-region (point-min)
                                            (point-max)
                                            'utf-8)
                      (goto-char (point-min))
                      (while (search-forward "\\u0000" nil t)
                        (replace-match "" nil t))
                      (goto-char (point-min))
                      (push (lsp-json-read-buffer) messages))

                  (error
                   (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                             (concat leftovers input)
                             err)))))))
        (mapc (lambda (msg)
                (lsp--parser-on-message msg workspace))
              (nreverse messages))))))
```

# Changelog

1. In `volar 0.30.0` you can skip calculate auto import by set `lsp-typescript-suggest-auto-imports` to `nil` to improve the completion speed. Check [this issue](https://github.com/johnsoncodehk/volar/issues/808#issuecomment-998895416).
   
   `lsp-typescript-suggest-auto-imports` is a setting registed by `lsp-vetur`.

   **This will disable the auto completation of third-party libraries and affect the experience, so you don't have to set it to `nil` unless you feel that there is a significant performance imporvement after disabling it** .

2. After `volar 0.32.1` `@volar/server` was renamed to `@volar/vue-language-server` , so you need resintall the new package.

# Language Server

1. `M-x lsp-install-server` select `volar-api` or `volar-doc` or `volar-html`
2.  Or use `npm/yarn` , `npm install -g @volar/vue-language-server`

# Customization

## lsp-volar

### `lsp-volar-take-over-mode`
It is enabled by default. If you want disable it, add `(setq lsp-volar-take-over-mode nil)` to your config file. [What is Take Over Mode?](https://github.com/johnsoncodehk/volar/discussions/471 "What is Take Over Mode?") 

### `lsp-volar-activate-file`
If your project don't have a `package.json` file with `vue` dependencies in `workspace-root`, such as a *yarn workspace* / *mono-repo* project, you can add a file with a custom name to force enable `@volar/server` for this project. The default file name is `.volarrc`. You can specify any file name by modifying the file `lsp-volar-activate-file` variable.

# Reference

1. [Tutorial: nvim-lspconfig - how to set up multiple language servers](https://github.com/johnsoncodehk/volar/discussions/606 "Tutorial: nvim-lspconfig - how to set up multiple language servers")  
2. [I have multiple language servers registered for language FOO. Which one will be used when opening a project?](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project "I have multiple language servers registered for language FOO. Which one will be used when opening a project?") 
3. [ I have multiple language servers for language FOO and I want to select the server per project, what can I do?](https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-for-language-foo-and-i-want-to-select-the-server-per-project-what-can-i-do " I have multiple language servers for language FOO and I want to select the server per project, what can I do?") 
4. [multiple servers](https://github.com/johnsoncodehk/volar/discussions/393#discussioncomment-1213736 "multiple servers") 
