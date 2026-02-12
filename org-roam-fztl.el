;;; org-roam-fztl.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fztl
;; Version: 0.14.2
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

;;; Folgezettel Operations

;; A folgezettel, mostly abbreviated to `fz', refers to folgezettel ID. The
;; underlying data for such an ID is a list of integers.

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

(defun org-roam-fztl-fz--from-id (&optional id extra)
  "Get folgezettels from ID.
If not given, ID defaults to the ID of current node.

See `org-roam-fztl--mapping-id2fz-get' for EXTRA."
  (org-roam-fztl--mapping-id2fz-get (or id (org-roam-id-at-point)) extra))

(defun org-roam-fztl-fz--to-id (fz)
  "Get ID for folgezettel FZ."
  (org-roam-fztl--mapping-fz2id-get fz))

(defun org-roam-fztl-fz--get-children (fz)
  "Get child folgezettel from FZ."
  (let (result)
    (if fz
        (let* ((fz (copy-sequence fz))
               (fz-child (org-roam-fztl-fz--resize fz (1+ (length fz)) 1)))
          (while (org-roam-fztl--mapping-fz2id-get fz-child)
            (push (copy-sequence fz-child) result)
            (org-roam-fztl-fz--lsd-inc fz-child)))
      ;; Top-level folgezettels could be non-contiguous.
      (maphash (lambda (k _)
                 (pcase-let ((`(,type ,fz) k))
                   (when (and (eq type 'fz) (= (length fz) 1))
                     (push (copy-sequence fz) result))))
               org-roam-fztl--mapping))
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
          (setf (alist-get fz fz-pos-items nil nil #'equal) pos)
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

(defun org-roam-fztl--mapping-id2fz-get (id &optional extra)
  "Get all folgezettels associated with ID from mapping storage.
When EXTRA is non-nil, return also outline ID and position in it."
  (when-let* ((vs (gethash `(id ,id) org-roam-fztl--mapping)))
    (let (result)
      (pcase-dolist (`(,outline-id . ,fz-pos-items) vs)
        (pcase-dolist (`(,fz . ,pos) fz-pos-items)
          (push (if extra `(,fz ,outline-id ,pos) fz) result)))
      result)))

(defun org-roam-fztl--mapping-init ()
  "Fill mapping storage from all outline nodes."
  (clrhash org-roam-fztl--mapping)
  (dolist (node (org-roam-fztl-node-outline-list))
    (let* ((buffer (find-file-noselect (org-roam-node-file node))))
      (with-current-buffer buffer
        (org-roam-fztl--mapping-from-outline-node (org-roam-node-id node))
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
       (outline-id (and (org-roam-fztl-node-outline-p node)
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
      (if-let* ((ids (seq-keep
                      (lambda (fz) (org-roam-fztl-fz--to-id fz))
                      (apply #'append
                             (seq-keep
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

(defun org-roam-fztl-node-outline-p (&optional node)
  "Return non-nil if NODE is folgezettel outline."
  (when-let* ((node (or node (org-roam-node-at-point)))
              (tags (org-roam-node-tags node)))
    (member org-roam-fztl-outline-tag tags)))

(defun org-roam-fztl-node-outline-list ()
  "Get folgezettel outline nodes."
  (seq-keep (lambda (node)
              (when (org-roam-fztl-node-outline-p node)
                node))
            (org-roam-node-list)))

;;; Outline Window

(defcustom org-roam-fztl-outline-window-layout '(top . 0.167)
  "Side window layout options."
  :type '(alist :key-type (choice (const :tag "Top" top)
                                  (const :tag "Right" right)
                                  (const :tag "Left" left)
                                  (const :tag "Bottom" bottom))
                :value-type number)
  :group 'org-roam-fztl)

(defvar org-roam-fztl-outline-window-layout--size-default 0.167)

(defun org-roam-fztl-outline-window-layout--current (&optional side)
  "Set window layout to SIDE or get currently active one.
When not given, SIDE defaults to the first entry in `org-roam-fztl-outline-window-layout'."
  ;; Ensure list of cons.
  (setq org-roam-fztl-outline-window-layout
        (cond ((null org-roam-fztl-outline-window-layout) nil)
              ((consp (car org-roam-fztl-outline-window-layout))
               org-roam-fztl-outline-window-layout)
              ((consp org-roam-fztl-outline-window-layout)
               (list org-roam-fztl-outline-window-layout))
              (t nil)))
  (if side
      (setq org-roam-fztl-outline-window-layout
            (if-let* ((v (assq side org-roam-fztl-outline-window-layout)))
                (cons v (delq v org-roam-fztl-outline-window-layout))
              (cons (cons side org-roam-fztl-outline-window-layout--size-default)
                    org-roam-fztl-outline-window-layout))))
  (car org-roam-fztl-outline-window-layout))

(defconst org-roam-fztl-outline-window--buffer-name " outline buffer"
  "Name of (indirect) buffer visiting outline node file.")

(defun org-roam-fztl-outline-window--get ()
  "Return outline window if it is in use."
  (get-buffer-window org-roam-fztl-outline-window--buffer-name t))

(defun org-roam-fztl-outline-window--display-buffer (buffer &optional side)
  "Display SIDE window for outline BUFFER.
This function returns the newly created side window."
  (pcase-let*
      ((`(,side . ,size)
        (org-roam-fztl-outline-window-layout--current side))
       (window-size
        (cons (if (member side '(top bottom))
                  #'window-height #'window-width)
              size)))
    (with-current-buffer buffer
      (when (org-roam-fztl-node-outline-p)
        (unless (derived-mode-p 'org-roam-fztl-outline-mode)
          (org-roam-fztl-outline-mode))))
    (display-buffer buffer
                    `(display-buffer-in-side-window
                      . ((side . ,side)
                         (slot . -1)
                         ,window-size
                         (dedicated . t)
                         (window-parameters
                          . ((no-delete-other-windows . t)
                             (no-other-window . t)
                             (mode-line-format . none)
                             (dedicated . t))))))))

(defun org-roam-fztl-outline-window--side (side)
  "Layout outline window to SIDE."
  (when-let* ((win (org-roam-fztl-outline-window--get)))
    (let ((buf (window-buffer win)))
      (delete-window win)
      (with-current-buffer buf
        (org-roam-fztl-outline-window--display-buffer buf side)))))

(defun org-roam-fztl-outline-window-top ()
  "Layout outline window to top."
  (interactive)
  (org-roam-fztl-outline-window--side 'top))

(defun org-roam-fztl-outline-window-right ()
  "Layout outline window to right."
  (interactive)
  (org-roam-fztl-outline-window--side 'right))

(defun org-roam-fztl-outline-window-botton ()
  "Layout outline window to bottom."
  (interactive)
  (org-roam-fztl-outline-window--side 'bottom))

(defun org-roam-fztl-outline-window-left ()
  "Layout outline window to left."
  (interactive)
  (org-roam-fztl-outline-window--side 'left))

(defun org-roam-fztl-outline-window-focus ()
  "Put focus on outline window.
When called from a folgezettel, the point will be moved to the entry in its
outline."
  (interactive)
  (when-let* ((win (org-roam-fztl-outline-window--get)))
    (if-let* ((node (and (derived-mode-p 'org-mode)
                         (org-roam-node-at-point)))
              (id (org-roam-node-id node))
              (items (org-roam-fztl-fz--from-id id 'extra)))
        (pcase-dolist (`(,fz ,outline-id ,pos) items)
          (let* ((outline-node (org-roam-node-from-id outline-id))
                 (file (org-roam-node-file outline-node))
                 (buf (window-buffer win)))
            (delete-window win)
            (kill-buffer buf)
            (setq win (org-roam-fztl-outline-window--display-buffer
                       (make-indirect-buffer
                        (find-file-noselect file)
                        org-roam-fztl-outline-window--buffer-name t)))
            (select-window win 'norecord)
            (goto-char pos)))
      (select-window win 'norecord))))

;;;###autoload
(defun org-roam-fztl-outline-window-on ()
  "Show outline window."
  (if-let* ((win (org-roam-fztl-outline-window--get)))
      nil                     ; do nothing if window exists
    (if-let* ((buf (get-buffer org-roam-fztl-outline-window--buffer-name)))
        (org-roam-fztl-outline-window--display-buffer buf)
      (when-let*
          ((options (seq-keep (lambda (node)
                                (when (org-roam-fztl-node-outline-p node)
                                  (cons (org-roam-node-title node) node)))
                              (org-roam-node-list)))
           (node (alist-get (completing-read "Folgezettel outline: "
                                             options nil t)
                            options nil nil #'equal))
           (file (org-roam-node-file node)))
        (org-roam-fztl-outline-window--display-buffer
         (make-indirect-buffer
          (find-file-noselect file)
          org-roam-fztl-outline-window--buffer-name t))))))

(defun org-roam-fztl-outline-window-off ()
  "Hide outline window."
  (when-let* ((win (org-roam-fztl-outline-window--get)))
    (delete-window win)
    t))

;;;###autoload
(defun org-roam-fztl-outline-window-toggle ()
  "Toggle outline window."
  (interactive)
  (unless (org-roam-fztl-outline-window-off)
    (org-roam-fztl-outline-window-on)))

;;; Major Mode (org-roam-fztl-outline-mode)

(defcustom org-roam-fztl-outline-tag "fztl"
  "Org tag for folgezettel outline node.
`org-roam-fztl' treats nodes given this tag as a folgezettel outline node."
  :type 'string
  :group 'org-roam-fztl)

(defcustom org-roam-fztl-outline-tags-exclude nil
  "Tags to exclude from being added to headlines in outline notes."
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
  (when (org-roam-fztl-node-outline-p)
    (if (member #'org-roam-fztl-outline-preview-async post-command-hook)
        (remove-hook 'post-command-hook #'org-roam-fztl-outline-preview-async t)
      (add-hook 'post-command-hook #'org-roam-fztl-outline-preview-async nil t))))

(defun org-roam-fztl-outline--headline-link ()
  "Get link on headline at point if exists."
  (when-let*
      ((raw (org-get-heading t t t t))
       (parsed (org-element-parse-secondary-string raw '(link))))
    (org-element-map parsed 'link #'identity nil t)))

(defun org-roam-fztl-outline--headline-linked-node ()
  "Get node linked on headline at point if exists."
  (when-let*
      ((lnk (org-roam-fztl-outline--headline-link))
       (id (and (equal (org-element-property :type lnk) "id")
                (org-element-property :path lnk)))
       (node (org-roam-node-from-id id)))
    node))

;; Show node title in minibuffer

(defcustom org-roam-fztl-outline-show-title 'minibuffer
  "Target to show node title at point.
Either nil or `minibuffer' is allowed."
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Do not show" nil))
  :group 'org-roam-fztl)

(defcustom org-roam-fztl-outline-show-title-delay 0.2
  "Delay before showing title in outline."
  :type 'number
  :group 'org-roam-fztl)

(defvar-local org-roam-fztl-outline-show-title--timer nil)

(defun org-roam-fztl-outline-show-title ()
  "Show note title in target specified in `org-roam-fztl-outline-show-title'."
  (when org-roam-fztl-outline-show-title
    (when (timerp org-roam-fztl-outline-show-title--timer)
      (cancel-timer org-roam-fztl-outline-show-title--timer))
    (when-let* ((lnk (org-roam-fztl-outline--headline-link))
                (contents (org-element-contents lnk))
                (desc (org-element-interpret-data contents)))
      (when (eq org-roam-fztl-outline-show-title 'minibuffer)
        (setq org-roam-fztl-outline-show-title--timer
              (run-with-idle-timer
               org-roam-fztl-outline-show-title-delay nil
               (lambda ()
                 (minibuffer-message "Note: %s" desc)
                 (setq org-roam-fztl-outline-show-title--timer nil))))))))

;; When outline buffer is modified, indicate with fringe color.

(defface org-roam-fztl-outline-modified
  '((((background dark))  :inherit region)
    (((background light)) :inherit region))
  "Fringe face for modified outline buffers.")

(defvar-local org-roam-fztl-outline-modified--cookie nil)

(defun org-roam-fztl-outline-modified--change-fringe ()
  "Indicate when outline buffer is modified.
Add this to `post-command-hook'."
  (if (buffer-modified-p)
      (setq-local org-roam-fztl-outline-modified--cookie
                  (face-remap-add-relative
                   'fringe 'org-roam-fztl-outline-modified))
    (when org-roam-fztl-outline-modified--cookie
      (face-remap-remove-relative org-roam-fztl-outline-modified--cookie)
      (set-window-fringes (selected-window) nil nil t)
      (setq-local org-roam-fztl-outline-modified--cookie nil))))

(defun org-roam-fztl-outline-org-return (&rest _rest)
  "Override `org-return' for faster navigation.
This command changes default behavior to find a link on current header and visit
if such a link exists."
  (interactive)
  (if-let* ((node (org-roam-fztl-outline--headline-linked-node)))
      (org-roam-node-visit node)
    (apply #'org-return _rest)))

(defun org-roam-fztl-outline-node-open ()
  "Open node at point in another window."
  (interactive)
  (when-let*
      ((node (org-roam-fztl-outline--headline-linked-node))
       (file (org-roam-node-file node))
       (buf (find-file-noselect file))
       (win (get-mru-window nil t t)))
    (display-buffer buf
                    `((
                       ;; display-buffer-in-previous-window
                       display-buffer-reuse-window
                       display-buffer-use-some-window)
                      (inhibit-same-window . t)
                      (window . ,win)))))

(defun org-roam-fztl-outline-switch-node ()
  "Switch to different outline node."
  (interactive)
  (org-roam-node-find nil nil #'org-roam-fztl-node-outline-p))

(defun org-roam-fztl-outline-edit ()
  "Edit outline node normally."
  (interactive)
  (let* ((node (org-roam-node-at-point))
         (file (org-roam-node-file node))
         (buf (find-file-noselect file))
         (win (get-mru-window nil t t)))
    (org-roam-fztl-outline-window-mode -1)
    (display-buffer buf
                    `((display-buffer-in-previous-window
                       display-buffer-reuse-window
                       display-buffer-use-some-window)
                      (inhibit-same-window . t)
                      (window . ,win)))
    (with-current-buffer buf
      (org-mode)
      (read-only-mode -1))))

(defmacro org-roam-fztl-outline--modify (&rest body)
  "Temporarily toggle `read-only-mode' while running BODY."
  `(let* ((inhibit-read-only t))
     (unwind-protect
         (progn
           (read-only-mode -1)
           ,@body
           (save-buffer))
       (read-only-mode +1))))

(defun org-roam-fztl-outline-insert-child ()
  "Insert child of current heading."
  (interactive)
  (org-roam-fztl-outline--modify
   (end-of-line)
   (org-meta-return)
   (org-do-demote)
   (org-roam-node-insert)
   (beginning-of-line)))

(defun org-roam-fztl-outline-insert-sibling ()
  "Insert sibling of current headline."
  (interactive)
  (org-roam-fztl-outline--modify
   (org-insert-heading-respect-content)
   (org-roam-node-insert)
   (beginning-of-line)))

(defun org-roam-fztl-outline-delete-subtree ()
  "Delete subtree of current headline."
  (interactive)
  (org-roam-fztl-outline--modify
   (beginning-of-line)
   (org-mark-subtree)
   (call-interactively #'kill-region)))

(defvar-keymap org-roam-fztl-outline-mode-map
  :doc "Keymap for `org-roam-fztl-mode' under prefix."
  ;; Similar to org-speed-command:
  "n" #'org-next-visible-heading
  "p" #'org-previous-visible-heading
  "f" #'org-forward-heading-same-level
  "b" #'org-backward-heading-same-level
  "u" #'outline-up-heading

  "C" #'org-roam-fztl-outline-insert-child
  "S" #'org-roam-fztl-outline-insert-sibling
  "D" #'org-roam-fztl-outline-delete-subtree
  "E" #'org-roam-fztl-outline-edit
  "V" #'org-roam-fztl-outline-preview-toggle
  "O" #'org-roam-fztl-outline-switch-node
  ;; "G" #'org-roam-fztl-outline-tags-refresh

  "v" #'org-roam-fztl-outline-org-return
  "<return>" #'org-roam-fztl-outline-org-return

  "o" #'org-roam-fztl-outline-node-open)

;;;###autoload
(define-derived-mode org-roam-fztl-outline-mode org-mode "fztl"
  "Major mode for folgezettel outline mode."
  :group 'org-roam
  (read-only-mode 1)
  (setq-local org-use-speed-commands nil)

  ;; (set-keymap-parent org-roam-fztl-outline-mode-map nil)
  (keymap-global-set "C-c f o" #'org-roam-fztl-outline-window-focus)

  (add-hook 'post-command-hook
            #'org-roam-fztl-outline-modified--change-fringe nil t)
  (add-hook 'after-save-hook #'org-roam-fztl--mapping-from-outline-node 98 t)
  (add-hook 'after-save-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'org-roam-post-node-insert-hook
            (lambda (id desc) (org-roam-fztl-outline-tags-refresh)) 99 t)
  (add-hook 'post-command-hook #'org-roam-fztl-outline-show-title nil t))

;;; Minor Mode (org-roam-fztl-mode)

(defcustom org-roam-fztl-mode-prefix "C-c f"
  "Prefix key sequence for `org-roam-fztl-mode' commands."
  :type 'string
  :group 'org-roam-fztl)

(defvar-keymap org-roam-fztl-mode-prefix-map
  :doc "Keymap for `org-roam-fztl-mode' under prefix."
  "a" #'org-roam-fztl-node-find
  "c" #'org-roam-fztl-node-find-children
  "p" #'org-roam-fztl-node-find-parents
  "s" #'org-roam-fztl-node-find-siblings
  "i c" #'org-roam-fztl-node-insert-child
  "i p" #'org-roam-fztl-node-insert-parent
  "i s" #'org-roam-fztl-node-insert-sibling)

(defvar-keymap org-roam-fztl-mode-map
  :doc "Keymap for `org-roam-fztl-mode'.")

(keymap-set org-roam-fztl-mode-map org-roam-fztl-mode-prefix org-roam-fztl-mode-prefix-map)

(defun org-roam-fztl-mode--on ()
  "Activate `org-roam-fztl-mode'."
  (add-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--mapping-init-maybe)
  (add-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh 99 t)
  (add-hook 'after-change-functions #'org-roam-fztl-overlay--refresh 99 t))

(defun org-roam-fztl-mode--off ()
  "Deactivate `org-roam-fztl-mode'."
  (remove-hook 'after-change-functions #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fztl-overlay--refresh t)
  (remove-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--mapping-init-maybe))

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "Minor mode for folgezettel support in Org Roam."
  :lighter " fztl"
  :group 'org-roam
  :keymap 'org-roam-fztl-mode-map
  (if org-roam-fztl-mode (org-roam-fztl-mode--on) (org-roam-fztl-mode--off)))

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
