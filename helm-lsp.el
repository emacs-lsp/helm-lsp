;;; helm-lsp.el --- LSP helm integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages, debug
;; URL: https://github.com/yyoncho/helm-lsp
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "5.0") (helm "2.0"))
;; Version: 0.2

;;; Commentary:

;; `helm' for lsp function.

;;; Code:

(require 'helm)
(require 'helm-imenu)
(require 'dash)
(require 'lsp-mode)

(defvar helm-lsp-symbols-request-id nil)
(defvar helm-lsp-symbols-result-p nil)
(defvar helm-lsp-symbols-result nil)

(defgroup helm-lsp nil
  "`helm-lsp' group."
  :group 'lsp-mode
  :tag "Language Server")

(lsp-defun helm-lsp-workspace-symbol-action ((&SymbolInformation :location
                                                                 (&Location :uri :range (&RangeToPoint :start))))
  "Action for helm workspace symbol.
CANDIDATE is the selected item in the helm menu."
  (find-file (lsp--uri-to-path uri))
  (goto-char start))

(defface helm-lsp-container-face
  '((t :height 0.8 :inherit shadow))
  "The face used for code lens overlays."
  :group 'helm-lsp)

(defcustom helm-lsp-treemacs-icons t
  "If non-nil, use `lsp-treemacs' icons."
  :group 'helm-lsp
  :type 'boolean)

(defun helm-lsp--extract-file-name (uri)
  (propertize
   (if (string= "jdt" (-> uri url-unhex-string url-generic-parse-url url-type))
       (cl-second (s-match ".*\(\\(.*\\)" uri))
     (f-filename uri))
   'face 'helm-lsp-container-face))

(defun helm-lsp--get-icon (kind)
  (require 'lsp-treemacs)
  (ht-get (treemacs-theme->gui-icons (treemacs--find-theme lsp-treemacs-theme))
          (lsp-treemacs-symbol-kind->icon kind)))

(defun helm-lsp--extract-file-name (uri)
  (propertize
   (if (string= "jdt" (-> uri url-unhex-string url-generic-parse-url url-type))
       (cl-second (s-match ".*\(\\(.*\\)" uri))
     (f-filename uri))
   'face 'helm-lsp-container-face))

(defun helm-lsp--workspace-symbol (workspaces name input)
  "Search against WORKSPACES NAME with default INPUT."
  (setq helm-lsp-symbols-result nil)
  (if workspaces
      (with-lsp-workspaces workspaces
        (helm
         :sources (helm-build-sync-source name
                    :candidates (lambda ()
                                  (if helm-lsp-symbols-result-p
                                      helm-lsp-symbols-result
                                    (with-lsp-workspaces workspaces
                                      (-let (((request &as &plist :id request-id) ))
                                        (setq helm-lsp-symbols-request-id request-id)
                                        (lsp-request-async
                                         "workspace/symbol"
                                         (list :query helm-pattern)
                                         (lambda (candidates)
                                           (setq helm-lsp-symbols-request-id nil)
                                           (and helm-alive-p
                                                (let ((helm-lsp-symbols-result-p t))
                                                  (setq helm-lsp-symbols-result candidates)
                                                  (helm-update))))
                                         :mode 'detached
                                         :cancel-token :workspace-symbols)
                                        helm-lsp-symbols-result))))
                    :action 'helm-lsp-workspace-symbol-action
                    :volatile t
                    :fuzzy-match t
                    :match (-const t)
                    :keymap helm-map
                    :candidate-transformer
                    (lambda (candidates)
                      (-map
                       (-lambda ((candidate &as
                                            &SymbolInformation :container-name? :name :kind :location (&Location :uri)))
                         (let ((type (or (alist-get kind lsp--symbol-kind) "Unknown")))
                           (cons
                            (if (and (featurep 'lsp-treemacs)
                                     helm-lsp-treemacs-icons)
                                (concat
                                 (or (helm-lsp--get-icon kind)
                                     (helm-lsp--get-icon 'fallback))
                                 (if (s-blank? container-name?)
                                     name
                                   (concat name " " (propertize container-name? 'face 'helm-lsp-container-face)))
                                 (propertize " · " 'face 'success)
                                 (helm-lsp--extract-file-name uri))
                              (concat (if (s-blank? container-name?)
                                          name
                                        (concat name " " (propertize container-name? 'face 'helm-lsp-container-face) " -" ))
                                      " "
                                      (propertize (concat "(" type ")") 'face 'font-lock-type-face)
                                      (propertize " · " 'face 'success)
                                      (helm-lsp--extract-file-name uri)))
                            candidate)))
                       (-take helm-candidate-number-limit candidates)))
                    :candidate-number-limit nil
                    :requires-pattern 0)
         :input input))
    (user-error "No LSP workspace active")))

;;;###autoload
(defun helm-lsp-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (helm-lsp--workspace-symbol (or (lsp-workspaces)
                                  (gethash (lsp-workspace-root default-directory)
                                           (lsp-session-folder->servers (lsp-session))))
                              "Workspace symbol"
                              (when arg (thing-at-point 'symbol))))

;;;###autoload
(defun helm-lsp-global-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (helm-lsp--workspace-symbol (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
                              "Global workspace symbols"
                              (when arg (thing-at-point 'symbol))))

;;;###autoload
(defun helm-lsp-code-actions()
  "Show lsp code actions using helm."
  (interactive)
  (let ((actions (lsp-code-actions-at-point)))
    (cond
     ((seq-empty-p actions) (signal 'lsp-no-code-actions nil))
     ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
      (lsp-execute-code-action (lsp-seq-first actions)))
     (t (helm :sources
              (helm-build-sync-source "Code Actions"
                :candidates actions
                :candidate-transformer
                (lambda (candidates)
                  (-map
                   (-lambda ((candidate &as
                                        &CodeAction :title))
                     (list title :data candidate))
                   candidates))
                :action '(("Execute code action" . (lambda(candidate)
                                                     (lsp-execute-code-action (plist-get candidate :data)))))))))))
(-let (((&Position :line :character) (lsp-make-position :character 10 :line 10)))
  character)


(defun helm-lsp-get-symbol-data (symbols path)
  (-mapcat (lambda (symbol)
             (cons (concat (lsp:document-symbol-name symbol) "|" path)
                   (helm-lsp-get-symbol-data (lsp:document-symbol-children? symbol)
                                             (concat path "." (lsp:document-symbol-name symbol))) ))
           symbols))

(defun helm-lsp-get-symbol-data-1 (symbols parents)
  (-mapcat (lambda (symbol)
             (cons (lsp-put symbol :_parents parents)
                   (helm-lsp-get-symbol-data-1 (lsp:document-symbol-children? symbol)
                                               (cl-list* symbol parents))))
           symbols))

;; (with-current-buffer "App.java"
;;   (helm-lsp-get-symbol-data-1 (lsp-request "textDocument/documentSymbol"
;;                                            `(:textDocument ,(lsp--text-document-identifier)))
;;                               nil)
;;   )
;; (spacemacs/set-leader-keys
;;   "aj" #'helm-lsp-symbols)
;; (with-current-buffer "App.java"
;;   (-some-> (helm :sources (helm-build-sync-source "test"
;;                             :candidate-transformer (lambda (candidates)
;;                                                      (-map (lambda (symbol)
;;                                                              (cons (concat
;;                                                                     (lsp-treemacs-get-icon (lsp-treemacs-symbol-kind->icon (lsp:document-symbol-kind symbol)))
;;                                                                     (s-pad-right  60 " " (lsp:document-symbol-name symbol))
;;                                                                     (propertize (mapconcat #'lsp:document-symbol-name
;;                                                                                            (lsp-get symbol :_parents) " · ")
;;                                                                                 'face 'font-lock-keyword-face)) symbol))
;;                                                            candidates))
;;                             :candidates (helm-lsp-get-symbol-data-1 (lsp-request "textDocument/documentSymbol"
;;                                                                                  `(:textDocument ,(lsp--text-document-identifier)))
;;                                                                     nil)

;;                             :action '(("Go to" . (lambda (candidate)
;;                                                    (-> candidate
;;                                                        lsp:document-symbol-selection-range
;;                                                        lsp:range-start
;;                                                        lsp--position-to-point
;;                                                        goto-char
;;                                                        )))
;;                                       ("Show references" . (lambda (candidate)
;;                                                              (save-excursion
;;                                                                (-> candidate
;;                                                                    lsp:document-symbol-selection-range
;;                                                                    lsp:range-start
;;                                                                    lsp--position-to-point
;;                                                                    goto-char)
;;                                                                (lsp-find-references))))
;;                                       ("Delete" . (lambda (_candidate)
;;                                                     (let ((marked (helm-marked-candidates) ))
;;                                                       (->> marked
;;                                                            (-filter (-lambda (symbol)
;;                                                                       (-none? (lambda (parent)
;;                                                                                 (-contains? symbols parent))
;;                                                                               (lsp-get symbol :_parents))))
;;                                                            (sort (lambda (left right)
;;                                                                    (> (-> left lsp:document-symbol-range lsp:range-start lsp--position-to-point)
;;                                                                       (-> right lsp:document-symbol-range lsp:range-start lsp--position-to-point))))
;;                                                            (mapc (lambda (symbol)
;;                                                                    (delete-region
;;                                                                     (-> symbol lsp:document-symbol-range lsp:range-start lsp--position-to-point)
;;                                                                     (-> symbol lsp:document-symbol-range lsp:range-end lsp--position-to-point)))))
;;                                                       (when (buffer-live-p helm-buffer)
;;                                                         (with-helm-buffer
;;                                                           (setq helm-marked-candidates nil
;;                                                                 helm-visible-mark-overlays nil))
;;                                                         (message "Deleted %s symbols(s)" (length marked))))))))
;;                  :buffer "*helm test*")
;;     lsp:document-symbol-selection-range
;;     lsp:range-start
;;     lsp--position-to-point
;;     goto-char))


;; (setq symbols my/symbols)
;; (length symbols)




;; (defun helm-lsp-symbols ()
;;   (interactive)
;;   (-some->
;;       (helm
;;        :sources
;;        (helm-build-sync-source "test"
;;          :candidate-transformer (lambda (candidates)
;;                                   (-map (lambda (symbol)
;;                                           (cons (concat
;;                                                  (lsp-treemacs-get-icon (lsp-treemacs-symbol-kind->icon (lsp:document-symbol-kind symbol)))
;;                                                  (s-pad-right  60 " " (lsp:document-symbol-name symbol))
;;                                                  (propertize (mapconcat #'lsp:document-symbol-name
;;                                                                         (lsp-get symbol :_parents) " · ")
;;                                                              'face 'font-lock-keyword-face)) symbol))
;;                                         candidates))
;;          :candidates (helm-lsp-get-symbol-data-1 (lsp-request "textDocument/documentSymbol"
;;                                                               `(:textDocument ,(lsp--text-document-identifier)))
;;                                                  nil)

;;          :action '(("Go to" . (lambda (candidate)
;;                                 (-> candidate
;;                                     lsp:document-symbol-selection-range
;;                                     lsp:range-start
;;                                     lsp--position-to-point
;;                                     goto-char)))
;;                    ("Show references" . (lambda (candidate)
;;                                           (save-excursion
;;                                             (-> candidate
;;                                                 lsp:document-symbol-selection-range
;;                                                 lsp:range-start
;;                                                 lsp--position-to-point
;;                                                 goto-char)
;;                                             (lsp-find-references))))
;;                    ("Show references" . (lambda (candidate)
;;                                           (save-excursion
;;                                             (-> candidate
;;                                                 lsp:document-symbol-selection-range
;;                                                 lsp:range-start
;;                                                 lsp--position-to-point
;;                                                 goto-char)
;;                                             (lsp-find-references))))
;;                    ("Rename" . (lambda (candidate)
;;                                  (save-excursion
;;                                    (-> candidate
;;                                        lsp:document-symbol-selection-range
;;                                        lsp:range-start
;;                                        lsp--position-to-point
;;                                        goto-char)
;;                                    (call-interactively #'lsp-rename))))
;;                    ("Delete" . (lambda (_candidate)
;;                                  (let ((marked (helm-marked-candidates) ))
;;                                    (mapc (lambda (symbol)
;;                                            (delete-region
;;                                             (-> symbol lsp:document-symbol-range lsp:range-start lsp--position-to-point)
;;                                             (-> symbol lsp:document-symbol-range lsp:range-end lsp--position-to-point)))
;;                                          (-sort (lambda (left right)
;;                                                   (> (-> left lsp:document-symbol-range lsp:range-start lsp--position-to-point)
;;                                                      (-> right lsp:document-symbol-range lsp:range-start lsp--position-to-point)))
;;                                                 (-filter (-lambda (symbol)
;;                                                            (-none? (lambda (parent)
;;                                                                      (-contains? symbols parent))
;;                                                                    (lsp-get symbol :_parents)))
;;                                                          marked)))
;;                                    (when (buffer-live-p helm-buffer)
;;                                      (with-helm-buffer
;;                                        (setq helm-marked-candidates nil
;;                                              helm-visible-mark-overlays nil))
;;                                      (message "Deleted %s symbols(s)" (length marked))))))))
;;        :buffer "*helm test*")
;;     lsp:document-symbol-selection-range
;;     lsp:range-start
;;     lsp--position-to-point
;;     goto-char))

;; (provide 'helm-lsp)
;; ;;; helm-lsp.el ends here
