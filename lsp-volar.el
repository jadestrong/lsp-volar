;;; lsp-volar.el --- A lsp-mode client for Vue3 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 JadeStrong
;;
;; Author: JadeStrong <https://github.com/jadestrong>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: November 08, 2021
;; Modified: November 08, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-volar
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;
;;; Commentary:
;;
;; provide the connection to lsp-mode and volar language server
;;
;;; Code:
(require 'lsp-mode)
(require 'json)

(defgroup lsp-volar nil
  "lsp support for vue3"
  :group 'lsp-volar)

(defcustom lsp-volar-take-over-mode t
  "Enable Take Over Mode."
  :type 'boolean
  :group 'lsp-volar)

(defun lsp-volar-get-typescript-server-path ()
  "Get tsserver.js file path."
  (if-let ((package-path (lsp-package-path 'typescript))
           (system-server-path (apply #'f-join (append (cl-subseq (f-split (file-truename package-path)) 0 -2) '("lib" "tsserver.js"))))
           (is-exist (f-file-p system-server-path)))
      system-server-path
    ""))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(lsp-dependency 'volar-language-server
                '(:system "volar-server")
                '(:npm :package "@volar/server" :path "volar-server"))

(lsp-register-custom-settings
 '(("typescript.serverPath" (lambda () (if-let ((project-root (lsp-workspace-root))
                                                (server-path (concat project-root "node_modules/typescript/lib/tsserverlibrary.js"))
                                                (is-exist (file-exists-p server-path)))
                                           server-path
                                        (lsp-volar-get-typescript-server-path))) t)
   ("languageFeatures.references" t t)
   ("languageFeatures.definition" t t)
   ("languageFeatures.typeDefinition" t t)
   ("languageFeatures.callHierarchy" t t)
   ("languageFeatures.hover" t t)
   ("languageFeatures.rename" t t)
   ("languageFeatures.renameFileRefactoring" t t)
   ("languageFeatures.signatureHelp" t t)
   ("languageFeatures.codeAction" t t)
   ("languageFeatures.completion.defaultTagNameCase" "both" t)
   ("languageFeatures.completion.defaultAttrNameCase" "kebabCase" t t)
   ("languageFeatures.completion.getDocumentNameCasesRequest" nil t)
   ("languageFeatures.completion.getDocumentSelectionRequest" nil t)
   ("languageFeatures.schemaRequestService" t t)

   ("documentFeatures.documentColor" nil t)
   ("documentFeatures.selectionRange" t t)
   ("documentFeatures.foldingRange" t t)
   ("documentFeatures.linkedEditingRange" t t)
   ("documentFeatures.documentSymbol" t t)
   ("documentFeatures.documentFormatting" 100 t)
   ("html.hover" t t)))

(defun lsp-volar-vite-vue-project-p (workspace-root)
  "Check if the 'vue' is preset in the package.json file when the project is a vite project."
  (if-let ((package-json (f-join workspace-root "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json))
           (dependencies (alist-get 'dependencies config)))
      (alist-get 'vue dependencies)
  nil))

(defun lsp-volar--activate-p (filename &optional _)
  "Check if the volar-language-server should be enabled base on FILENAME."
  (if lsp-volar-take-over-mode
      (and (or (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vue.config.js")))
               (locate-dominating-file (buffer-file-name) "vue.config.js")
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vue.config.ts")))
               (locate-dominating-file (buffer-file-name) "vue.config.ts")
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vite.config.js")) (lsp-volar-vite-vue-project-p (lsp-workspace-root)))
               (if-let ((js-workspace-root (locate-dominating-file (buffer-file-name) "vite.config.js"))) (lsp-volar-vite-vue-project-p js-workspace-root) nil)
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) "vite.config.ts")))
               (if-let ((ts-workspace-root (locate-dominating-file (buffer-file-name) "vite.config.ts"))) (lsp-volar-vite-vue-project-p ts-workspace-root) nil)
               (and (lsp-workspace-root) (f-file-p (f-join (lsp-workspace-root) ".volarrc"))))
           (or (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
                   (and (derived-mode-p 'js-mode 'typescript-mode)
                        (not (derived-mode-p 'json-mode))))
               (string= (file-name-extension filename) "vue")))
   (string= (file-name-extension filename) "vue")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :server-id 'volar-api
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (lsp-configuration-section "html")
                                               (lsp-configuration-section "languageFeatures")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "html")
                                 (lsp-configuration-section "languageFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-language-server
                                            callback error-callback))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :add-on? t
  :server-id 'volar-doc
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (ht ("languageFeatures" (ht-merge (ht ("semanticTokens" nil))
                                                                                 (ht ("documentHighlight" t))
                                                                                 (ht ("documentLink" t))
                                                                                 (ht ("codeLens" t))
                                                                                 (ht ("diagnostics" t)))))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "languageFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-language-server
                                            callback error-callback))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'volar-language-server) "--stdio")))
  :activation-fn 'lsp-volar--activate-p
  :priority 0
  :multi-root nil
  :add-on? t
  :server-id 'volar-html
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (lsp-configuration-section "documentFeatures")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (ht-merge (lsp-configuration-section "typescript")
                                 (lsp-configuration-section "documentFeatures")))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-language-server
                                            callback error-callback))))

(provide 'lsp-volar)
;;; lsp-volar.el ends here
