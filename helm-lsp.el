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

(defun helm-lsp-workspace-symbol-action (candidate)
  "Action for helm workspace symbol.
CANDIDATE is the selected item in the helm menu."
  (let* ((location (gethash "location" candidate))
         (start (gethash "start" (gethash "range" location))))
    (find-file (lsp--uri-to-path (gethash "uri" location)))
    (goto-char (point-min))
    (forward-line (1+ (gethash "line" start)))
    (forward-char (gethash "character" start))))

(defun helm-lsp-workspace-symbol (arg)
  "Preconfigured `helm' for lsp workspace/symbol.

When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (lsp--cur-workspace-check)
  (let ((workspace lsp--cur-workspace))
    (helm
     :sources
     (helm-build-sync-source "*workspace-symbol*"
       :candidates (lambda ()
                     (let ((lsp--cur-workspace workspace))
                       (mapcar
                        (lambda (symbol)
                          (cons (format "%s.%s"
                                        (gethash "containerName" symbol)
                                        (gethash "name" symbol))
                                symbol))
                        (lsp-send-request (lsp-make-request "workspace/symbol"
                                                            (list :query helm-pattern))))))
       :action 'helm-lsp-workspace-symbol-action
       :volatile t
       :keymap helm-map)
     :input (thing-at-point 'symbol))))

(provide 'helm-lsp)
;;; helm-lsp.el ends here
