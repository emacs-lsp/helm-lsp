;;; helm-lsp.el --- LSP helm integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan

;; Author: Ivan <kyoncho@myoncho>
;; Keywords:

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

;;; Commentary:

;;

;;; Code:

(require 'helm)
(require 'lsp-methods)

(defvar lsp-helm-request-id nil)
(defvar helm-lsp-result-p nil)
(defvar helm-lsp-result nil)

(defun helm-lsp-workspace-symbol-action (candidate)
  "Action for helm workspace symbol.
CANDIDATE is the selected item in the helm menu."
  (let* ((location (gethash "location" candidate))
         (start (gethash "start" (gethash "range" location))))
    (find-file (lsp--uri-to-path (gethash "uri" location)))
    (goto-char (point-min))
    (forward-line (gethash "line" start))
    (forward-char (gethash "character" start))))

(defun helm-lsp-workspace-symbol (arg)
  "Preconfigured `helm' for lsp workspace/symbol.

When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (lsp--cur-workspace-check)

  (let ((workspace lsp--cur-workspace))
    (helm
     :sources (helm-build-sync-source "Workspace symbol"
                :candidates (lambda ()
                              (if helm-lsp-result-p
                                  helm-lsp-result
                                (let* ((lsp--cur-workspace workspace)
                                       (request (lsp-make-request "workspace/symbol"
                                                                  (list :query helm-pattern)))
                                       (request-id        (plist-get request :id)))

                                  ;; cancel if there is pending request
                                  (when lsp-helm-request-id
                                    (lsp--cancel-request lsp-helm-request-id)
                                    (setq lsp-helm-request-id nil))

                                  (setq lsp-helm-request-id request-id)
                                  (lsp-send-request-async
                                   request
                                   (lambda (candidates)
                                     (setq lsp-helm-request-id nil)
                                     (and helm-alive-p
                                          (let ((helm-lsp-result candidates)
                                                (helm-lsp-result-p t))
                                            (helm-update)))))
                                  nil)))
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

     :input (when arg (thing-at-point 'symbol)))))

(provide 'helm-lsp)
;;; helm-lsp.el ends here
