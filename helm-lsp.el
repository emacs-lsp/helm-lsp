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
;; Version: 0.1

;;; Commentary:

;; `helm' for lsp function.

;;; Code:

(require 'helm)
(require 'dash)
(require 'lsp-mode)

(defvar helm-lsp-symbols-request-id nil)
(defvar helm-lsp-symbols-result-p nil)
(defvar helm-lsp-symbols-result nil)

(defun helm-lsp-workspace-symbol-action (candidate)
  "Action for helm workspace symbol.
CANDIDATE is the selected item in the helm menu."
  (-let* (((&hash "uri" "range" (&hash "line" "character")) (gethash "location" candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))


(defmacro with-lsp-workspaces (workspaces &rest body)
  "Helper macro for invoking BODY against WORKSPACES."
  (declare (debug (form body))
           (indent 1))
  `(let ((lsp--buffer-workspaces ,workspaces)) ,@body))

(defun help-lsp--workspace-symbol (workspaces name input)
  "Search against WORKSPACES NAME with default INPUT."
  (if workspaces
      (helm
       :sources (helm-build-sync-source name
                  :candidates (lambda ()
                                (if helm-lsp-symbols-result-p
                                    helm-lsp-symbols-result
                                  (with-lsp-workspaces workspaces
                                    (-let (((request &as &plist :id request-id) (lsp-make-request
                                                                                 "workspace/symbol"
                                                                                 (list :query helm-pattern))))
                                      ;; cancel if there is pending request
                                      (when helm-lsp-symbols-request-id
                                        (lsp--cancel-request helm-lsp-symbols-request-id)
                                        (setq helm-lsp-symbols-request-id nil))

                                      (setq helm-lsp-symbols-request-id request-id)
                                      (lsp-send-request-async
                                       request
                                       (lambda (candidates)
                                         (setq helm-lsp-symbols-request-id nil)
                                         (and helm-alive-p
                                              (let ((helm-lsp-symbols-result candidates)
                                                    (helm-lsp-symbols-result-p t))
                                                (helm-update))))
                                       'detached)
                                      nil))))
                  :action 'helm-lsp-workspace-symbol-action
                  :volatile t
                  :keymap helm-map
                  :candidate-transformer (lambda (candidates)
                                           (cl-loop for c in candidates
                                                    collect (cons (format "%s.%s"
                                                                          (gethash "containerName" c)
                                                                          (gethash "name" c))
                                                                  c)))
                  :candidate-number-limit nil
                  :requires-pattern 1)
       :input input)
    (user-error "No LSP workspace active")))

(defun helm-lsp-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (help-lsp--workspace-symbol (lsp-workspaces)
                              "Workspace symbol"
                              (when arg (thing-at-point 'symbol))))

(defun helm-lsp-global-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (help-lsp--workspace-symbol (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
                              "Global workspace symbols"
                              (when arg (thing-at-point 'symbol))))

(provide 'helm-lsp)
;;; helm-lsp.el ends here
