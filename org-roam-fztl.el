;;; org-roam-fztl.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fztl
;; Version: 0.3.2
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
;; This Org Roam plugin provides support for folgezettel.
;;
;;; Code:

(require 'org-roam)

(defgroup org-roam-fztl nil
  "Settings for `org-roam-fztl'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/org-roam-fztl"))

;;; Outline Documents

(defcustom org-roam-fztl-outline-nodes nil
  "Folgezettel outline nodes.
Each item in this list is a cons cell of the form `(NODE-ID . START)', where
NODE-ID is the ID of node containing an Folgezettel outline, and START is the
starting integer of Folgezettel sequence."
  :type '(repeat cons)
  :group 'org-roam-fztl)

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

(defun org-roam-fztl-fz--resize (fz n &optional initval)
  "Resize folgezettel FZ to N digits.
If given, fill new digit(s) with INITVAL (defaults to zero)."
  (let ((initval (or initval 0))
        (m (length fz)))
    (cond ((< n m) (seq-take fz n))
          ((< m n) (append fz (make-list (- n m) initval)))
          (t fz))))

(defun org-roam-fztl-fz--lsd-inc (fz)
  "Increment least-significant digit of folgezettel FZ."
  (let ((lsd (1- (length fz))))
    (setcar (nthcdr lsd fz) (1+ (nth lsd fz)))))

(defun org-roam-fztl-fz--from-id (&optional id)
  "Get folgezettels from ID.
If not given, ID defaults to the ID of current node."
  (org-roam-fztl--mapping-id2fz-get (or id (org-roam-id-at-point))))

(defun org-roam-fztl-fz--to-id (fz)
  "Get ID for folgezettel FZ."
  (org-roam-fztl--mapping-fz2id-get fz))

(defun org-roam-fztl-fz--get-children (fz)
  "Get child folgezettel from FZ."
  (let* ((fz (copy-sequence fz))
         (fz-child (org-roam-fztl-fz--resize fz (1+ (length fz)) 1))
         result)
    (while (org-roam-fztl--mapping-fz2id-get fz-child)
      (push (copy-sequence fz-child) result)
      (org-roam-fztl-fz--lsd-inc fz-child))
    result))

(defun org-roam-fztl-fz--get-parents (fz)
  "Get parents for FZ."
  (list (butlast fz)))

(defun org-roam-fztl-fz--get-siblings (fz)
  "Get siblings for FZ."
  (apply #'append
         (mapcar (lambda (fz)
                   (org-roam-fztl-fz--get-children fz))
                 (org-roam-fztl-fz--get-parents fz))))

;;; Mapping Storage for ID-Folgezettel Relations

(defvar org-roam-fztl--mapping (make-hash-table :test #'equal)
  "Mapping storage.
Each mapping entry is `(TYPE KEY)' as key and VALUE. This holds mapping both
from ID to FZ and the reverse, FZ to ID. TYPE is a symbol (either `id' or `fz')
specifying whether KEY is ID or FZ.")

(defun org-roam-fztl--mapping-empty-p ()
  "Return non-nil if mapping storage is empty."
  (= (hash-table-count org-roam-fztl--mapping) 0))

(defun org-roam-fztl--mapping-clear ()
  "Empty mapping storage."
  (interactive)
  (clrhash org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-put (id fz outline-id)
  "Put relation between ID and FZ into mapping storage.
OUTLINE-ID is the ID of outline node."
  (let ((key `(id ,id))
        (value (cons outline-id fz)))
    (if-let* ((items (gethash key org-roam-fztl--mapping)))
        (progn
          (setf (alist-get outline-id items nil nil #'equal) fz)
          (puthash key items org-roam-fztl--mapping))
      (puthash key (list value) org-roam-fztl--mapping)))
  (puthash `(fz ,fz) (cons id outline-id) org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-remove (id fz)
  "Remove relation between ID and FZ from mapping storage."
  (remhash `(id ,id) org-roam-fztl--mapping)
  (remhash `(fz ,fz) org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-fz2id-get (fz)
  "Get ID for FZ from mapping storage."
  (when-let* ((v (gethash `(fz ,fz) org-roam-fztl--mapping)))
    (car v)))

(defun org-roam-fztl--mapping-id2fz-get (id)
  "Get all folgezettels associated with ID from mapping storage."
  (when-let* ((vs (gethash `(id ,id) org-roam-fztl--mapping)))
    (mapcar (lambda (v) (cdr v)) vs)))

(defun org-roam-fztl--mapping-init ()
  "Fill mapping storage from all outline nodes."
  (clrhash org-roam-fztl--mapping)
  (pcase-dolist (`(,id . ,start) org-roam-fztl-outline-nodes)
    (with-current-buffer
        (find-file-noselect (org-roam-node-file (org-roam-node-from-id id)))
      (org-roam-fztl--mapping-from-outline-node))))

(defun org-roam-fztl--mapping-from-outline-node ()
  "Parse current outline buffer to update mapping storage."
  (when-let* ((outline-id (org-roam-node-id (org-roam-node-at-point)))
              (start (cdr (assoc outline-id org-roam-fztl-outline-nodes))))
    (let ((stage (make-hash-table :test #'equal))
          (fz `(,start)))
      ;; Stage existing ID-folgezettel mapping.
      (maphash (lambda (k v)
                 (pcase-let ((`(,type ,id) k))
                   (when-let* ((_ (eq type 'id))
                               (fz (alist-get outline-id v nil nil #'equal)))
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
           (setq fz (org-roam-fztl-fz--resize fz level))
           (org-roam-fztl-fz--lsd-inc fz)

           (when id
             (if-let* ((v-stage (gethash id stage)))
                 (progn
                   (when (not (equal fz v-stage))
                     (org-roam-fztl--mapping-put id (copy-sequence fz) outline-id))
                   (remhash id stage))
               (org-roam-fztl--mapping-put id (copy-sequence fz) outline-id))))))

      ;; Remove staged entries that no longer exist.
      (maphash (lambda (id fz) (org-roam-fztl--mapping-remove id fz)) stage))))

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
  (when-let* ((fzs (org-roam-fztl--mapping-id2fz-get id)))
    (string-join
     (mapcar (lambda (fz)
               (format org-roam-fztl-overlay-fz-format
                       (org-roam-fztl-fz--render fz)))
             fzs)
     "")))

(defun org-roam-fztl-overlay--in-title ()
  "Put folgezettel overlays in node title."
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

;;; Nodes

(defun org-roam-fztl-node-has-fz-p ()
  "Return non-nil if node at point is folgezettel."
  (and (org-roam-fztl-fz--from-id) t))

(defun org-roam-fztl-node-find ()
  "Find and open folgezettel node."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node)
                        (org-roam-fztl-fz--from-id (org-roam-node-id node)))))

(defun org-roam-fztl-node--op (type filter-fn)
  "Perform operation of TYPE on folgezettel nodes.
The function FILTER-FN takes a folgezettel and returns related folgezettels."
  (if (org-roam-fztl-node-has-fz-p)
      (if-let* ((ids (mapcar
                      (lambda (fz) (org-roam-fztl-fz--to-id fz))
                      (apply #'append
                             (mapcar
                              (lambda (fz) (funcall filter-fn fz))
                              (org-roam-fztl-fz--from-id))))))
          (pcase type
            ('find
             (org-roam-node-find nil nil
                                 (lambda (node)
                                   (member (org-roam-node-id node) ids))))
            ('insert
             (org-roam-node-insert (lambda (node)
                                     (member (org-roam-node-id node) ids)))))
        (message "No folgezettel nodes found"))
    (warn "Node is not folgezettel")))

(defun org-roam-fztl-node-find-children ()
  "Find and open children of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'find #'org-roam-fztl-fz--get-children))

(defun org-roam-fztl-node-find-parents ()
  "Find and open parents of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'find #'org-roam-fztl-fz--get-parents))

(defun org-roam-fztl-node-find-siblings ()
  "Find and open siblings of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'find #'org-roam-fztl-fz--get-siblings))

(defun org-roam-fztl-node-insert-child ()
  "Insert Org link to child of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'insert #'org-roam-fztl-fz--get-children))

(defun org-roam-fztl-node-insert-parent ()
  "Insert Org link to parent of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'insert #'org-roam-fztl-fz--get-parents))

(defun org-roam-fztl-node-insert-sibling ()
  "Insert Org link to sibling of folgezettel at point."
  (interactive)
  (org-roam-fztl-node--op 'insert #'org-roam-fztl-fz--get-siblings))

(defun org-roam-fztl-node-jump-to-outline ()
  "Jump to outline entry for folgezettel at point."
  (interactive)
  (when-let* ((id (org-roam-node-id (org-roam-node-at-point)))
              (pattern (format "\\[\\[id:%s\\]\\(\\[[^]]+\\]\\)?\\]" id)))
    (when-let*
        ((result
          (catch 'done
            (pcase-dolist (`(,id . ,_) org-roam-fztl-outline-nodes)
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

;;; Minor Mode Configuration

(defun org-roam-fztl-mode--activate ()
  "Activate `org-roam-fztl-mode'."
  ;; (add-hook 'window-configuration-change-hook #'org-roam-fztl--overlay-refresh 99 t)
  (add-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-node 98 t)
  (add-hook 'after-save-hook #'org-roam-fztl-overlay--refresh 99 t)

  (global-set-key (kbd org-roam-fztl-prefix) org-roam-fztl-mode-map))

(defun org-roam-fztl-mode--deactivate ()
  "Deactivate `org-roam-fztl-mode'."
  (global-unset-key (kbd org-roam-fztl-prefix))

  (remove-hook 'after-save-hook #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-node t)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh t))

(defcustom org-roam-fztl-prefix "C-c f"
  "Prefix key sequence for `org-roam-fztl-mode' commands."
  :type 'string
  :group 'org-roam-fztl)

(defvar-keymap org-roam-fztl-mode-map
  :doc "Keymap for `org-roam-fztl-mode'."
  "a" #'org-roam-fztl-node-find
  "c" #'org-roam-fztl-node-find-children
  "p" #'org-roam-fztl-node-find-parents
  "s" #'org-roam-fztl-node-find-siblings
  "i c" #'org-roam-fztl-node-insert-child
  "i p" #'org-roam-fztl-node-insert-parent
  "i s" #'org-roam-fztl-node-insert-sibling
  "o" #'org-roam-fztl-node-jump-to-outline)

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "Minor mode for folgezettel support in Org Roam."
  :lighter " fztl"
  :group 'org-roam
  :keymap org-roam-fztl-mode-map
  (if org-roam-fztl-mode
      (org-roam-fztl-mode--activate)
    (org-roam-fztl-mode--deactivate)))

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
