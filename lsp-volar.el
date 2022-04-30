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
;; Package-Requires: ((emacs "25.1"))
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
  "Lsp support for vue3."
  :group 'lsp-volar)

(defcustom lsp-volar-take-over-mode t
  "Enable Take Over Mode."
  :type 'boolean
  :group 'lsp-volar)

(defcustom lsp-volar-activate-file ".volarrc"
  "A file with a custom name placed in WORKSPACE-ROOT is used to force enable
 volar when there is no package.json in the WORKSPACE-ROOT."
  :type 'string
  :group 'lsp-volar)

(defcustom lsp-volar-completion-tag-casing "both"
  "Casing conversion for tag completion."
  :type '(choice
          (const "both")
          (const "kebabCase")
          (const "pascalCase"))
  :group 'lsp-volar
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-volar-completion-attr-casing "kebabCase"
  "Casing conversion for attr completion."
  :type '(choice
          (const "kebabCase")
          (const "camelCase"))
  :group 'lsp-volar
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-volar-inlay-hints nil
  "Show inlay hints."
  :type 'boolean
  :group 'lsp-volar
  :package-version '(lsp-mode . "8.0.1"))

(defconst lsp-volar--is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defun lsp-volar-get-typescript-server-path ()
  "Get tsserver.js file path."
  (if-let ((package-path (lsp-package-path 'typescript))
           (system-server-path (apply #'f-join (if lsp-volar--is-windows
                                                   (append (cl-subseq (f-split (file-truename (lsp-package-path 'typescript))) 0 -1) '("node_modules" "typescript" "lib" "tsserver.js"))
                                                 (append (cl-subseq (f-split (file-truename (lsp-package-path 'typescript))) 0 -2) '("lib" "tsserver.js")))))
           (is-exist (f-file-p system-server-path)))
      system-server-path
    (progn (lsp--error "[lsp-volar] Typescript is not detected correctly. Please ensure the npm package typescript is installed in your project or system (npm install -g typescript), otherwise open an issue") "")))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                       :path "tsserver"))

(lsp-dependency 'volar-language-server
                '(:system "vue-language-server")
                '(:npm :package "@volar/vue-language-server" :path "vue-language-server"))

(lsp-register-custom-settings
 '(("typescript.serverPath" (lambda () (if-let ((project-root (lsp-workspace-root))
                                                (server-path (f-join project-root "node_modules/typescript/lib/tsserverlibrary.js"))
                                                (is-exist (file-exists-p server-path)))
                                           server-path
                                        (lsp-volar-get-typescript-server-path))) t)
   ("typescript.inlayHints.enumMemberValues.enabled" lsp-javascript-display-enum-member-value-hints t)
   ("typescript.inlayHints.functionLikeReturnTypes.enabled" lsp-javascript-display-return-type-hints t)
   ("typescript.inlayHints.parameterNames.enabled" lsp-javascript-display-parameter-name-hints t)
   ("typescript.inlayHints.parameterTypes.enabled" lsp-javascript-display-parameter-type-hints t)
   ("typescript.inlayHints.propertyDeclarationTypes.enabled" lsp-javascript-display-property-declaration-type-hints t)
   ("typescript.inlayHints.variableTypes.enabled" lsp-javascript-display-variable-type-hints t)

   ("javascript.inlayHints.enumMemberValues.enabled" lsp-javascript-display-enum-member-value-hints t)
   ("javascript.inlayHints.functionLikeReturnTypes.enabled" lsp-javascript-display-return-type-hints t)
   ("javascript.inlayHints.parameterNames.enabled" lsp-javascript-display-parameter-name-hints t)
   ("javascript.inlayHints.parameterTypes.enabled" lsp-javascript-display-parameter-type-hints t)
   ("javascript.inlayHints.propertyDeclarationTypes.enabled" lsp-javascript-display-property-declaration-type-hints t)
   ("javascript.inlayHints.variableTypes.enabled" lsp-javascript-display-variable-type-hints t)

   ("languageFeatures.references" t t)
   ("languageFeatures.implementation" t t)
   ("languageFeatures.definition" t t)
   ("languageFeatures.typeDefinition" t t)
   ("languageFeatures.callHierarchy" t t)
   ("languageFeatures.hover" t t)
   ("languageFeatures.rename" t t)
   ("languageFeatures.renameFileRefactoring" t t)
   ("languageFeatures.signatureHelp" t t)
   ("languageFeatures.codeAction" t t)
   ("languageFeatures.workspaceSymbol" t t)
   ("languageFeatures.completion.defaultTagNameCase" lsp-volar-completion-tag-casing t)
   ("languageFeatures.completion.defaultAttrNameCase" lsp-volar-completion-attr-casing t)
   ("languageFeatures.completion.getDocumentNameCasesRequest" nil t)
   ("languageFeatures.completion.getDocumentSelectionRequest" nil t)
   ("languageFeatures.schemaRequestService.getDocumentContentRequest" nil t)

   ("documentFeatures.selectionRange" t t)
   ("documentFeatures.foldingRange" t t)
   ("documentFeatures.linkedEditingRange" t t)
   ("documentFeatures.documentSymbol" t t)
   ("documentFeatures.documentColor" t t)
   ("documentFeatures.documentFormatting.defaultPrintWidth" 100 t)
   ("documentFeatures.documentFormatting.getDocumentPrintWidthRequest" nil t)))

(defun lsp-volar--vue-project-p (workspace-root)
  "Check if the 'vue' package is present in the package.json file
in the WORKSPACE-ROOT."
  (if-let ((package-json (f-join workspace-root "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json))
           (dependencies (alist-get 'dependencies config)))
      (alist-get 'vue dependencies)
  nil))

(defun lsp-volar--activate-p (filename &optional _)
  "Check if the volar-language-server should be enabled base on FILENAME."
  (if lsp-volar-take-over-mode
      (and (or
            (and (lsp-workspace-root) (lsp-volar--vue-project-p (lsp-workspace-root)))
            (and (lsp-workspace-root) lsp-volar-activate-file (f-file-p (f-join (lsp-workspace-root) lsp-volar-activate-file))))
           (or (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
                   (and (derived-mode-p 'js-mode 'typescript-mode)
                        (not (derived-mode-p 'json-mode))))
               (string= (file-name-extension filename) "vue")))
   (string= (file-name-extension filename) "vue")))

(defun lsp-volar-initialized? ()
  (when-let ((workspace (lsp-find-workspace 'volar-doc (buffer-file-name))))
    (eq 'initialized (lsp--workspace-status workspace))))

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
                                               (lsp-configuration-section "languageFeatures")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[,(lsp-make-file-system-watcher :glob-pattern "**/*.js")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.ts")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.vue")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.jsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.tsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.json")])))))
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
                                               (ht ("languageFeatures" (ht-merge (ht ("documentHighlight" t))
                                                                                 (ht ("documentLink" t))
                                                                                 (ht ("codeLens" (ht ("showReferencesNotification" t))))
                                                                                 (ht ("semanticTokens" t))
                                                                                 (ht ("inlayHints" lsp-volar-inlay-hints))
                                                                                 (ht ("diagnostics" (ht ("getDocumentVersionRequest" nil))))
                                                                                 (ht ("schemaRequestService" (ht ("getDocumentContentRequest" nil)))))))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[,(lsp-make-file-system-watcher :glob-pattern "**/*.js")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.ts")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.vue")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.jsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.tsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.json")])))))
  :after-open-fn (lambda ()
                   (when lsp-volar-inlay-hints
                     (lsp-volar-inlay-hints-mode)))
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
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[,(lsp-make-file-system-watcher :glob-pattern "**/*.js")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.ts")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.vue")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.jsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.tsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.json")])))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'volar-language-server
                                            callback error-callback))))

;; inlay hints

(defvar-local lsp-volar-inlay-hints-timer nil)

(defun lsp-volar-update-inlay-hints (buffer)
  "Update inlay hints of the BUFFER."
  (if (and (lsp-volar-initialized?)
           (eq buffer (current-buffer)))
      (lsp-request-async
       "textDocument/inlayHint"
       (lsp-make-javascript-inlay-hints-params
        :text-document (lsp--text-document-identifier)
        :range (if (use-region-p)
                   (lsp--region-to-range (region-beginning) (region-end))
                 (lsp--region-to-range (point-min) (point-max))))
       (lambda (res)
         (remove-overlays (point-min) (point-max) 'lsp-volar-inlay-hint t)
         (dolist (hint res)
           (-let* (((&rust-analyzer:InlayHint :position :label :kind :padding-left :padding-right) hint)
                   (pos (lsp--position-to-point position))
                   (overlay (make-overlay pos pos nil 'front-advance 'end-advance)))
             (overlay-put overlay 'lsp-volar-inlay-hint t)
             (overlay-put overlay 'before-string
                          (format "%s%s%s"
                                        (if (and padding-left (not (eql kind lsp/rust-analyzer-inlay-hint-kind-type-hint))) " " "")
                                        (propertize (lsp-volar-format-inlay label kind)
                                                    'font-lock-face (lsp-javascript-face-for-inlay kind))
                                        (if padding-right " " "")))))))))
(defun lsp-volar-format-inlay (label kind)
  (cond
   ((eql kind lsp/rust-analyzer-inlay-hint-kind-type-hint) (format lsp-javascript-inlay-type-format label))
   ((eql kind lsp/rust-analyzer-inlay-hint-kind-parameter-hint) (format lsp-javascript-inlay-param-format label))
   ;; ((eql kind lsp/javascript-inlay-hint-kind-enum-hint) (format lsp-javascript-inlay-enum-format text))
   (t label)))


(defun lsp-volar-inlay-hints-change-handler (&rest _rest)
  (when lsp-volar-inlay-hints-timer
    (cancel-timer lsp-volar-inlay-hints-timer))
  (setq lsp-volar-inlay-hints-timer
        (run-with-idle-timer 0.1 nil #'lsp-volar-update-inlay-hints (current-buffer))))

(define-minor-mode lsp-volar-inlay-hints-mode
  "Mode for displaying inlay hints."
  :lighter nil
  (cond
   (lsp-volar-inlay-hints-mode
    (lsp-volar-update-inlay-hints (current-buffer))
    (add-hook 'lsp-on-change-hook #'lsp-volar-inlay-hints-change-handler nil t))
   (t
    (remove-overlays (point-min) (point-max) 'lsp-volar-inlay-hint t)
    (remove 'lsp-on-change-hook #'lsp-volar-inlay-hints-change-handler t))))

(provide 'lsp-volar)
;;; lsp-volar.el ends here
