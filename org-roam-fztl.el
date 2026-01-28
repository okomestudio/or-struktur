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

(defgroup org-roam-fztl nil
  "Settings for `org-roam-fztl'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/org-roam-fztl"))

;;; Outline Documents

(defcustom org-roam-fztl-outline-notes nil
  "Folgezettel outline notes.
Each item in this list is a cons cell of the form `(NOTE-ID . START)', where
NOTE-ID is the ID of note containing an Folgezettel outline, and START is the
starting integer of Folgezettel sequence."
  :type '(repeat cons)
  :group 'org-roam-fztl)

(defun org-roam-fztl-outline-jump-to-entry ()
  "Jump to outline entry for current note."
  (interactive)
  (when-let* ((id (org-roam-node-id (org-roam-node-at-point)))
              (pattern (format "\\[\\[id:%s\\]\\(\\[[^]]+\\]\\)?\\]" id)))
    (when-let*
        ((result
          (catch 'done
            (pcase-dolist (`(,id . ,_) org-roam-fztl-outline-notes)
              (when-let* ((node (org-roam-node-from-id id)))
                (with-current-buffer
                    (find-file-noselect (org-roam-node-file node))
                  (goto-char (point-min))
                  (when (re-search-forward pattern nil t)
                    (throw 'done `(,node ,(point))))))))))
      (pcase-let ((`(,node ,pt) result))
        (org-roam-node-visit node t t)
        (goto-char pt)
        (org-reveal)
        (recenter)))))

;;; ID-Folgezettel Mapping

(defvar org-roam-fztl--mapping (make-hash-table :test #'equal)
  "Mapping storage.
Each mapping entry is `(TYPE KEY)' as key and VALUE. This holds mapping both
from ID to FZ and the reverse, FZ to ID. TYPE is a symbol (either `id' or `fz')
specifying whether KEY is ID or FZ.")

(defun org-roam-fztl--mapping-put (id fz outline-id)
  "Put relation between ID and FZ into mapping storage.
OUTLINE-ID is the ID of outline note."
  (puthash `(id ,id) `(,fz ,outline-id) org-roam-fztl--mapping)
  (puthash `(fz ,fz) `(,id ,outline-id) org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-get (type id)
  "Get ID of TYPE (as symbol) from mapping storage.
TYPE is either `id' or `fz'."
  (when-let* ((value (gethash `(,type ,id) org-roam-fztl--mapping)))
    (pcase-let ((`(,id ,outline-id) value))
      id)))

(defun org-roam-fztl--mapping-remove (id fz)
  "Remove relation between ID and FZ from mapping storage."
  (remhash `(id ,id) org-roam-fztl--mapping)
  (remhash `(fz ,fz) org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-empty-p ()
  "Return non-nil if mapping storage is empty."
  (= (hash-table-count org-roam-fztl--mapping) 0))

(defun org-roam-fztl--mapping-clear ()
  "Empty mapping storage."
  (clrhash org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-init ()
  "Fill mapping storage from all outline notes."
  (clrhash org-roam-fztl--mapping)
  (dolist (each org-roam-fztl-outline-notes)
    (pcase-let ((`(,id . ,start) each))
      (with-current-buffer
          (find-file-noselect (org-roam-node-file (org-roam-node-from-id id)))
        (org-roam-fztl--mapping-from-outline-note)))))

(defun org-roam-fztl--mapping-from-outline-note ()
  "Parse current outline buffer to update mapping storage."
  (when-let* ((outline-id (org-roam-node-id (org-roam-node-at-point)))
              (start (cdr (assoc outline-id org-roam-fztl-outline-notes))))
    (let ((stage (make-hash-table :test #'equal)) fz)
      (maphash (lambda (key value)
                 (pcase-let ((`(,type ,id) key)
                             (`(,fz ,oid) value))
                   (when (and (string= oid outline-id)
                              (eq type 'id))
                     (puthash id fz stage))))
               org-roam-fztl--mapping)

      (org-map-entries
       (lambda ()
         (let* ((level (org-outline-level))
                (raw-title (org-get-heading t t t t))
                (parsed (org-element-parse-secondary-string raw-title '(link)))
                (link (org-element-map parsed 'link #'identity nil t))
                (link-type (and link (org-element-property :type link)))
                (link-path (and link (org-element-property :path link)))
                (id (and (equal link-type "id") link-path)))
           ;; Resize list representation of folgezettel ...
           (setq fz (cond
                     ((< level (length fz))
                      (seq-take fz level))
                     ((< (length fz) level)
                      (append fz
                              (make-list (- level (length fz))
                                         (if (= level 1) start 0))))
                     (t fz)))
           ;; ... and increment least significant digit.
           (setcar (nthcdr (1- level) fz) (1+ (nth (1- level) fz)))

           (when id
             (if-let* ((val-stage (gethash id stage)))
                 (progn
                   (when (not (equal fz val-stage))
                     (org-roam-fztl--mapping-put id (copy-sequence fz) outline-id))
                   (remhash id stage))
               (org-roam-fztl--mapping-put id (copy-sequence fz) outline-id))))))

      (maphash (lambda (id fz) (org-roam-fztl--mapping-remove id fz)) stage))))

;;; Folgezettel Operations

(defcustom org-roam-fztl-fz-format '("%d" ".%d")
  "Folgezettel format for its string representation.
Allowed formatters are '%d' (numeric) and '%s' (alphabetic)."
  :type '(list string)
  :group 'org-roam-fztl)

(defun org-roam-fztl-fz--number-to-alpha (n)
  "Convert positive N to alphabet representation.
The return value is a string or nil if N is not a positive integer."
  (unless (and (integerp n) (> n 0))
    (error "N must be a positive integer"))
  (let ((result "")
        (num n))
    (while (> num 0)
      (let* ((adjusted (1- num))
             (remainder (mod adjusted 26))
             (letter (char-to-string (+ ?a remainder))))
        (setq result (concat letter result))
        (setq num (/ adjusted 26))))
    result))

(defun org-roam-fztl-fz--render (fz)
  "Render folgezettel FZ using preset format.
The preset format is set with `org-roam-fztl-fz-format'."
  (string-join
   (cl-loop with (converter) = '(nil)
            for i below (length fz)
            collect
            (let ((fmt (or (nth i org-roam-fztl-fz-format)
                           (if (eq converter #'identity) "%s" "%d"))))
              (setq converter (or (and (string-search "%d" fmt) #'identity)
                                  #'org-roam-fztl-fz--number-to-alpha))
              (format fmt (funcall converter (nth i fz)))))))

(defun org-roam-fztl-fz--get (&optional id)
  "Get folgezettel for ID.
If not given, ID defaults to the ID of current note."
  (org-roam-fztl--mapping-get 'id (or id (org-roam-id-at-point))))

(defun org-roam-fztl-fz--get-parent (&optional id)
  "Get parent folgezettel for ID.
If not given, ID defaults to the ID of current note."
  (when-let* ((fz (org-roam-fztl-fz--get id)))
    (butlast fz)))

;;; Overlay Management

(defface org-roam-fztl-overlay
  `((t :inherit fixed-pitch
       :foreground ,(face-attribute 'shadow :foreground)
       :background ,(face-attribute 'shadow :background)))
  "Face used for fztl overlays.")

(defcustom org-roam-fztl-overlay-text-placement 'before-string
  "Specify side of overlay to show rendered folgezettel."
  :type '(choice 'before-string 'after-string)
  :group 'org-roam-fztl)

(defun org-roam-fztl-overlay--put (beg end text)
  "Add TEXT for overlay from BEG to END."
  (let* ((ov (make-overlay beg end))
         (propertized-text (propertize text 'face 'org-roam-fztl-overlay))
         (text (pcase org-roam-fztl-overlay-text-placement
                 ('before-string (concat propertized-text " "))
                 ('after-string (concat " " propertized-text)))))
    (overlay-put ov org-roam-fztl-overlay-text-placement text)
    (overlay-put ov 'category 'fztl)
    (overlay-put ov 'evaporate t)))

(defcustom org-roam-fztl-overlay-fz-format "[%s]"
  "String format used for rendered folgezettel in overlays."
  :type 'string
  :group 'org-roam-fztl)

(defun org-roam-fztl-overlay--format (id)
  "Render folgezettel from ID for overlay."
  (if-let* ((fz (org-roam-fztl--mapping-get 'id id))
            (styled-fz (org-roam-fztl-fz--render fz)))
      (format org-roam-fztl-overlay-fz-format styled-fz)))

(defun org-roam-fztl-overlay--in-title ()
  "Put folgezettel overlays in note title."
  (save-excursion
    (goto-char (point-min))
    (let* ((id (org-roam-id-at-point))
           (rendered (org-roam-fztl-overlay--format id)))
      (when (and rendered
                 (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t))
        (org-roam-fztl-overlay--put (match-beginning 1)
                                    (match-end 1)
                                    rendered)))))

(defun org-roam-fztl-overlay--in-headlines ()
  "Put folgezettel overlays in headlines.
IDs are extracted from headline properties."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (let* ((id (org-element-property :ID headline))
             (rendered (org-roam-fztl-overlay--format id)))
        (when rendered
          (org-roam-fztl--overlay-put
           (+ (org-element-property :begin headline)
              (org-element-property :level headline)
              1)
           (org-element-property :end headline)
           rendered))))))

(defun org-roam-fztl-overlay--in-links ()
  "Put folgezettel overlays in Org links."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (let* ((id (org-element-property :path link))
               (rendered (org-roam-fztl-overlay--format id)))
          (when rendered
            (org-roam-fztl-overlay--put (org-element-property :begin link)
                                        (org-element-property :end link)
                                        rendered)))))))

(defun org-roam-fztl-overlay--add ()
  "Put folgezettel overlays in current buffer."
  (when (org-roam-buffer-p)
    (org-roam-fztl-overlay--in-title)
    (org-roam-fztl-overlay--in-headlines)
    (org-roam-fztl-overlay--in-links)))

(defun org-roam-fztl-overlay--remove ()
  "Remove folgezettel overlays in current buffer."
  (when (org-roam-buffer-p)
    (remove-overlays (point-min) (point-max) 'category 'fztl)))

(defun org-roam-fztl-overlay--refresh ()
  "Refresh folgezettel overlays in current buffer."
  (when (org-roam-buffer-p)
    (when (org-roam-fztl--mapping-empty-p)
      (org-roam-fztl--mapping-init))
    (org-roam-fztl-overlay--remove)
    (org-roam-fztl-overlay--add)))

;;; Public API

(defun org-roam-fztl-node-parent-insert ()
  "Insert Org link to folgezettel parent node."
  (interactive)
  (when-let* ((fz-parent (org-roam-fztl-fz--get-parent))
              (id (org-roam-fztl--mapping-get 'fz fz-parent))
              (node (org-roam-node-from-id id))
              (desc (org-roam-node-title node)))
    (insert (org-link-make-string (concat "id:" id) desc))
    (run-hook-with-args 'org-roam-post-node-insert-hook id desc)))

;;; Minor Mode Configuration

(defun org-roam-fztl-mode--activate ()
  "Activate `org-roam-fztl-mode'."
  ;; (add-hook 'window-configuration-change-hook #'org-roam-fztl--overlay-refresh 99 t)
  (add-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-note 98 t)
  (add-hook 'after-save-hook #'org-roam-fztl-overlay--refresh 99 t))

(defun org-roam-fztl-mode--deactivate ()
  "Deactivate `org-roam-fztl-mode'."
  (remove-hook 'after-save-hook #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-note t)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh t))

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "Minor mode for folgezettel support in Org Roam."
  :lighter " fztl"
  :group 'org-roam
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-i") #'org-roam-fztl-outline-jump-to-entry)
            map)
  (if org-roam-fztl-mode
      (org-roam-fztl-mode--activate)
    (org-roam-fztl-mode--deactivate)))

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
