;;; org-roam-fztl.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fztl
;; Version: 0.8.4
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

(require 'org)
(require 'org-roam)

(defgroup org-roam-fztl nil
  "Settings for `org-roam-fztl'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/org-roam-fztl"))

;;; Outline Documents

(defcustom org-roam-fztl-outline-tag "fztl"
  "Org tag for folgezettel outline node.
`org-roam-fztl' treats nodes given this tag as a folgezettel outline node."
  :type 'string
  :group 'org-roam-fztl)

(defun org-roam-fztl-outline-p (&optional node)
  "Return non-nil if NODE is folgezettel outline."
  (when-let* ((node (or node (org-roam-node-at-point)))
              (tags (org-roam-node-tags node)))
    (member org-roam-fztl-outline-tag tags)))

(defun org-roam-fztl-outline-nodes ()
  "Get folgezettel outline nodes as alist.
The returned alist has the node ID of folgezettel outline as key and folgezettel
starting number as value."
  (seq-keep
   (lambda (node)
     (when (org-roam-fztl-outline-p node)
       (org-roam-node-id node)))
   (org-roam-node-list)))

(defcustom org-roam-fztl-outline-tags-exclude nil
  "Tags to exclude from outlines."
  :type '(repeat string)
  :group 'org-roam-fztl)

(defun org-roam-fztl-outline-tags-refresh ()
  "Refresh tags in headline at point in outline node.
Use `org-roam-fztl-outline-tags-exclude' to exclude tags from being added."
  (interactive)
  (when (not (org-at-heading-p))
    (warn "Point not on Org headline"))
  (let* ((raw-title (org-get-heading t t t t))
         (parsed (org-element-parse-secondary-string raw-title '(link)))
         (link (org-element-map parsed 'link #'identity nil t))
         (link-type (and link (org-element-property :type link)))
         (link-path (and link (org-element-property :path link)))
         (id (and (equal link-type "id") link-path)))
    (when-let* ((node (org-roam-node-from-id id))
                (tags (cl-set-difference (org-roam-node-tags node)
                                         org-roam-fztl-outline-tags-exclude
                                         :test #'equal)))
      (org-set-tags tags))))

(defun org-roam-fztl-outline-tags-refresh-all ()
  "Refresh tags in all headlines in outline node.
On each headline, refresh is performed by `org-roam-fztl-outline-tags-refresh'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries #'org-roam-fztl-outline-tags-refresh)))

(defun org-roam-fztl-outline-preview ()
  "Open node linked in current headline."
  (when-let* ((raw-title (org-get-heading t t t t))
              (parsed (org-element-parse-secondary-string raw-title '(link)))
              (link (org-element-map parsed 'link #'identity nil t))
              (link-type (and link (org-element-property :type link)))
              (link-path (and link (org-element-property :path link)))
              (id (and (equal link-type "id") link-path))
              (node (org-roam-node-from-id id)))
    (display-buffer (find-file-noselect (org-roam-node-file node))
                    '((display-buffer-use-some-window)
                      (inhibit-same-window . t)))))

(defun org-roam-fztl-outline-preview-async ()
  "Async-open node linked in current headline."
  (when (org-at-heading-p)
    (run-with-idle-timer 0.05 nil #'org-roam-fztl-outline-preview)))

(defun org-roam-fztl-outline-preview-toggle ()
  "Toggle outline preview."
  (interactive)
  (when (org-roam-fztl-outline-p)
    (if (member #'org-roam-fztl-outline-preview-async post-command-hook)
        (remove-hook 'post-command-hook #'org-roam-fztl-outline-preview-async t)
      (add-hook 'post-command-hook #'org-roam-fztl-outline-preview-async nil t))))

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

;; NOTE: The storage uses a hash table as a proof-of-concept implementation. For
;; scalability, consider using a relational database, e.g., SQLite.

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

(defun org-roam-fztl--mapping-put (id fz outline-id pos)
  "Put relation between ID and FZ into mapping storage.
OUTLINE-ID is the ID of outline node. POS is the outline node position."
  (let* ((fz (copy-sequence fz))
         (key `(id ,id))
         (value (cons outline-id (list (cons fz pos)))))
    (if-let* ((vs (gethash key org-roam-fztl--mapping)))
        (let ((fz-pos-items (alist-get outline-id vs nil nil #'equal)))
          (setf (alist-get fz fz-pos-items nil nil #'equal) (cons fz pos))
          (setf (alist-get outline-id vs nil nil #'equal) fz-pos-items)
          (puthash key vs org-roam-fztl--mapping))
      (puthash key (list value) org-roam-fztl--mapping))
    (puthash `(fz ,fz) (cons id outline-id) org-roam-fztl--mapping)))

(defun org-roam-fztl--mapping-remove (id fz)
  "Remove relation between ID and FZ from mapping storage."
  (when-let* ((vs (gethash `(id ,id) org-roam-fztl--mapping)))
    (pcase-dolist (`(,outline-id . ,fz-pos-items) vs)
      (let ((fz-pos-items-size (length fz-pos-items)))
        (pcase-dolist (`(,-fz . ,pos) fz-pos-items)
          (when (equal fz -fz)
            (setq fz-pos-items (assoc-delete-all fz fz-pos-items #'equal))))
        (if (= (length fz-pos-items) 0)
            (setq vs (assoc-delete-all outline-id vs #'equal))
          (if (/= (length fz-pos-items) fz-pos-items-size)
              (setf (alist-get outline-id vs nil nil #'equal) fz-pos-items)))))
    (if (= (length vs) 0)
        (remhash `(id ,id) org-roam-fztl--mapping)
      (puthash `(id ,id) vs org-roam-fztl--mapping)))
  (remhash `(fz ,fz) org-roam-fztl--mapping))

(defun org-roam-fztl--mapping-fz2id-get (fz)
  "Get ID for FZ from mapping storage."
  (when-let* ((v (gethash `(fz ,fz) org-roam-fztl--mapping)))
    (car v)))

(defun org-roam-fztl--mapping-id2fz-get (id)
  "Get all folgezettels associated with ID from mapping storage."
  (when-let* ((vs (gethash `(id ,id) org-roam-fztl--mapping)))
    (let (result)
      (pcase-dolist (`(,outline-id . ,fz-pos-items) vs)
        (pcase-dolist (`(,fz . ,pos) fz-pos-items)
          (push fz result)))
      result)))

(defun org-roam-fztl--mapping-init ()
  "Fill mapping storage from all outline nodes."
  (clrhash org-roam-fztl--mapping)
  (dolist (id (org-roam-fztl-outline-nodes))
    (let* ((node (org-roam-node-from-id id))
           (buffer (find-file-noselect (org-roam-node-file node))))
      (with-current-buffer buffer
        (org-roam-fztl--mapping-from-outline-node id)
        (org-roam-fztl-overlay--refresh)))))

(defun org-roam-fztl--mapping-init-maybe ()
  "If mapping storage is empty, initialize."
  (when (org-roam-fztl--mapping-empty-p)
    (org-roam-fztl--mapping-init)))

;;; TODO(2026-02-05): Improve the algorithm by performing update on affected
;;; tree.
(defun org-roam-fztl--mapping-from-outline-node (&optional outline-id)
  "Parse current outline buffer to update mapping storage."
  (when-let*
      ((node (or (and outline-id (org-roam-node-from-id outline-id))
                 (save-excursion (goto-char (point-min))
                                 (org-roam-node-at-point))))
       (outline-id (and (org-roam-fztl-outline-p node)
                        (org-roam-node-id node)))
       (start (string-to-number
               (or (cdr (assoc "FZTL_START" (org-roam-node-properties node)))
                   "0")))
       (stage (make-hash-table :test #'equal))
       (fz `(,start)))
    ;; Stage existing ID-folgezettel mapping.
    (maphash
     (lambda (k vs)
       (pcase-let ((`(,type ,id) k))
         (when-let* ((_ (eq type 'id))
                     (fz-pos-items (alist-get outline-id vs nil nil #'equal)))
           (pcase-dolist (`(,fz . ,pos) fz-pos-items)
             (puthash (cons id fz) pos stage)))))
     org-roam-fztl--mapping)

    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elmt)
        (let ((level (org-element-property :level elmt)))
          (setq fz (org-roam-fztl-fz--resize fz level))
          (org-roam-fztl-fz--lsd-inc fz)
          (when-let* ((lnk (org-element-map
                               (org-element-property :title elmt)
                               'link #'identity nil 'first-match))
                      (type (org-element-property :type lnk))
                      (id (and (equal type "id")
                               (org-element-property :path lnk)))
                      (pos (org-element-property :begin lnk)))
            (when (gethash (cons id fz) stage)
              (remhash (cons id fz) stage))
            (org-roam-fztl--mapping-put id fz outline-id pos)))))

    ;; Remove staged entries that no longer exist.
    (maphash (lambda (key _)
               (pcase-let* ((`(,id . ,fz) key))
                 (org-roam-fztl--mapping-remove id fz)))
             stage)))

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
  "Add TEXT overlay for content from BEG to END."
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
    (string-join (mapcar (lambda (fz)
                           (format org-roam-fztl-overlay-fz-format
                                   (org-roam-fztl-fz--render fz)))
                         fzs)
                 "")))

(defmacro org-roam-fztl-overlay--region (beg end len &rest body)
  "Initialize BEG, END, and LEN before running BODY.
BODY is not run if LEN is nil or zero. If BEG or END is nil, the bound will be
set to the whole buffer with `point-min' or `point-max'."
  (declare (indent defun))
  `(when (or (null len) (> len 0))
     (setq beg (or beg (point-min)) end (or end (point-max)))
     (save-excursion
       (save-restriction
         (narrow-to-region beg end)
         ,@body))))

(defun org-roam-fztl-overlay--render-in-title (&optional beg end len)
  "Render folgezettel overlays in document title.
The title is obtained from `#+title:'. For BEG, END, and LEN, see
`org-roam-fztl-overlay--region'."
  (org-roam-fztl-overlay--region beg end len
    (goto-char beg)
    (when-let* ((node (org-roam-node-at-point))
                (ovt (org-roam-fztl-overlay--format (org-roam-node-id node))))
      (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" end t)
        (org-roam-fztl-overlay--put (match-beginning 1) (match-end 1) ovt)))))

(defun org-roam-fztl-overlay--render-in-headlines (&optional beg end len)
  "Render folgezettel overlays in headlines.
IDs are extracted from headline properties. For BEG, END, and LEN, see
`org-roam-fztl-overlay--region'."
  (org-roam-fztl-overlay--region beg end len
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elmt)
        (when-let* ((id (org-element-property :ID elmt))
                    (ovt (org-roam-fztl-overlay--format id))
                    (beg (org-element-property :title-begin elmt))
                    (end (org-element-property :title-end elmt)))
          (org-roam-fztl-overlay--put beg end ovt))))))

(defun org-roam-fztl-overlay--render-in-links (&optional beg end len)
  "Render folgezettel overlays in Org links.
For BEG, END, and LEN, see `org-roam-fztl-overlay--region'."
  (org-roam-fztl-overlay--region beg end len
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (elmt)
        (when-let* ((id (and (string= (org-element-property :type elmt) "id")
                             (org-element-property :path elmt)))
                    (ovt (org-roam-fztl-overlay--format id))
                    (beg (org-element-property :begin elmt))
                    (end (org-element-property :end elmt)))
          (org-roam-fztl-overlay--put beg end ovt))))))

(defun org-roam-fztl-overlay--remove (&optional beg end len)
  "Remove folgezettel overlays in region.
For BEG, END, and LEN, see `org-roam-fztl-overlay--region'."
  (org-roam-fztl-overlay--region beg end len
    (remove-overlays beg end 'category 'fztl)))

(defun org-roam-fztl-overlay--refresh (&optional beg end len)
  "Refresh folgezettel overlays in region.
For BEG, END, and LEN, see `org-roam-fztl-overlay--region'."
  (org-roam-fztl-overlay--remove beg end len)
  (org-roam-fztl-overlay--render-in-title beg end len)
  (org-roam-fztl-overlay--render-in-headlines beg end len)
  (org-roam-fztl-overlay--render-in-links beg end len))

;;; Nodes

(defun org-roam-fztl-node-has-fz-p (&optional node)
  "Return non-nil if NODE has folgezettel.
If not given, NODE will be node at point."
  (and (org-roam-fztl-fz--from-id (and node (org-roam-node-id node))) t))

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
            (dolist (id (org-roam-fztl-outline-nodes))
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

(defcustom org-roam-fztl-prefix "C-c f"
  "Prefix key sequence for `org-roam-fztl-mode' commands."
  :type 'string
  :group 'org-roam-fztl)

(defvar-keymap org-roam-fztl-prefix-map
  :doc "Keymap for `org-roam-fztl-minor-mode'."
  "a" #'org-roam-fztl-node-find
  "c" #'org-roam-fztl-node-find-children
  "p" #'org-roam-fztl-node-find-parents
  "s" #'org-roam-fztl-node-find-siblings
  "i c" #'org-roam-fztl-node-insert-child
  "i p" #'org-roam-fztl-node-insert-parent
  "i s" #'org-roam-fztl-node-insert-sibling
  "o" #'org-roam-fztl-node-jump-to-outline)

(keymap-set org-mode-map org-roam-fztl-prefix org-roam-fztl-prefix-map)

(defun org-roam-fztl-mode--activate ()
  "Activate `org-roam-fztl-mode'."
  (add-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--mapping-init-maybe)
  (add-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'after-change-functions #'org-roam-fztl-overlay--refresh 99 t))

(defun org-roam-fztl-mode--deactivate ()
  "Deactivate `org-roam-fztl-mode'."
  (remove-hook 'after-change-functions #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--mapping-init-maybe))

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "Minor mode for folgezettel support in Org Roam."
  :lighter " fztl"
  :group 'org-roam
  (if org-roam-fztl-mode
      (org-roam-fztl-mode--activate)
    (org-roam-fztl-mode--deactivate)))

;;; Major Mode

(define-derived-mode org-roam-fztl-outline-mode org-mode "fztl/outline"
  "Major mode for folgezettel outline mode."
  (setq mode-name "fztl/outline")
  (add-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-node 98 t)
  (add-hook 'after-save-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'org-roam-post-node-insert-hook
            (lambda (id desc) (org-roam-fztl-outline-tags-refresh)) 99 t)
  (use-local-map org-roam-fztl-outline-mode-map))

(defvar org-roam-fztl-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `org-roam-fztl-outline-mode'.")

(defvar org-roam-fztl-outline-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (keymap-lookup org-mode-map org-roam-fztl-prefix))
    map)
  "Keymap for `org-roam-fztl-outline-mode'.")

(define-key org-roam-fztl-outline-mode-prefix-map "t" #'org-roam-fztl-outline-tags-refresh)
(define-key org-roam-fztl-outline-mode-prefix-map "v" #'org-roam-fztl-outline-preview-toggle)
(define-key org-roam-fztl-outline-mode-map (kbd org-roam-fztl-prefix) org-roam-fztl-outline-mode-prefix-map)

(defun org-roam-fztl-outline-mode--maybe-activate ()
  "Activate `org-roam-fztl-outline-mode' if buffer is folgezettel outline."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward (format "^#\\+filetags?:.*\\<%s\\>"
                                          org-roam-fztl-outline-tag)
                                  nil t)))
    (unless (derived-mode-p 'org-roam-fztl-outline-mode)
      (org-roam-fztl-outline-mode))))

(add-hook 'org-mode-hook #'org-roam-fztl-outline-mode--maybe-activate)

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
