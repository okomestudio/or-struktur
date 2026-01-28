;;; org-roam-fztl.el --- Folgezettel Plugin for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fztl
;; Version: 0.1.1
;; Keywords: org-roam, convenience
;; Package-Requires: ((emacs "30.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This Org Roam plugin provides support for Folgezettel ID.
;;
;;; Code:

(require 'org-roam)

;;; Minor Mode Configuration

(defun org-roam-fztl-mode--activate ()
  "Activate `org-roam-fztl-mode'.")

(defun org-roam-fztl-mode--deactivate ()
  "Deactivate `org-roam-fztl-mode'.")

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "The org-roam-fztl minor mode."
  :group 'org-roam
  :lighter "org-roam-fztl-mode"
  (if org-roam-fztl-mode
      (org-roam-fztl-mode--activate)
    (org-roam-fztl-mode--deactivate)))

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
