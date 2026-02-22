;;; org-roam-fztl.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fztl
;; Version: 0.17.1
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

;;; Utilities

(defmacro org-roam-fztl--debounce (delay &rest body)
  "Run BODY after DELAY seconds of idle time, debouncing repeated invocations."
  (declare (indent 1) (debug t))
  (let ((timer-var (make-symbol (format "debounce-timer-%d" (sxhash body)))))
    `(progn
       (when (bound-and-true-p ,timer-var)
         (cancel-timer ,timer-var))
       (set (make-local-variable ',timer-var)
            (run-with-idle-timer
             ,delay nil
             (lambda ()
               ,@body))))))

(defmacro org-roam-fztl--disable-command (mode command)
  "Disable COMMAND in major MODE."
  (let* ((command-s
          (replace-regexp-in-string "[\\#']" "" (format "%s" command)))
         (mode-s (replace-regexp-in-string "'" "" (format "%s" mode)))
         (fun (make-symbol (format "%s--disable-in-%s" command-s mode-s))))
    `(progn
       (defun ,fun (&rest _)
         (when (derived-mode-p ',mode)
           (user-error "%s is disabled in %s" ,command-s ,mode-s)))
       (advice-add ,command :before #',fun))))

(defmacro org-roam-fztl--narrow-and-eval (beg end buffer &rest body)
  "Narrow to region from BEG to END of BUFFER and evaluate BODY."
  (declare (indent 3))
  `(with-current-buffer (or buffer (current-buffer))
     (let ((beg (or ,beg (point-min)))
           (end (or ,end (point-max))))
       (when (> (- end beg) 1)
         (save-excursion
           (save-restriction
             (narrow-to-region beg end)
             ,@body))))))

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
  (org-roam-fztl--db-id2fz-get (or id (org-roam-id-at-point)) extra))

(defun org-roam-fztl-fz--to-id (fz)
  "Get ID for folgezettel FZ."
  (org-roam-fztl--db-fz2id-get fz))

(defun org-roam-fztl-fz--get-children (fz)
  "Get child folgezettel from FZ."
  (let (result)
    (if fz
        (let* ((fz (copy-sequence fz))
               (fz-child (org-roam-fztl-fz--resize fz (1+ (length fz)) 1)))
          (while (org-roam-fztl--db-fz2id-get fz-child)
            (push (copy-sequence fz-child) result)
            (org-roam-fztl-fz--lsd-inc fz-child)))
      ;; Top-level folgezettels could be non-contiguous.
      (maphash (lambda (k _)
                 (pcase-let ((`(,type ,fz) k))
                   (when (and (eq type 'fz) (= (length fz) 1))
                     (push (copy-sequence fz) result))))
               org-roam-fztl--db))
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

(defvar org-roam-fztl--db (make-hash-table :test #'equal)
  "Mapping storage.")

(defun org-roam-fztl--db-empty-p ()
  "Return non-nil if mapping storage is empty."
  (= (hash-table-count org-roam-fztl--db) 0))

(defun org-roam-fztl--db-clear ()
  "Empty mapping storage."
  (interactive)
  (clrhash org-roam-fztl--db))

(defun org-roam-fztl--db-fz2id-get (fz)
  "Get ID for FZ from mapping storage."
  (when-let* ((v (gethash `(fz ,fz) org-roam-fztl--db)))
    (car v)))

(defun org-roam-fztl--db-id2fz-get (id &optional extra)
  "Get all folgezettels associated with ID from mapping storage.
When EXTRA is non-nil, return also outline ID and position in it."
  (apply #'append
         (seq-keep
          (lambda (outline-id)
            (when-let*
                ((db (gethash `(outline ,outline-id)
                              org-roam-fztl--db)))
              (mapcar
               (lambda (item)
                 (pcase-let* ((`(,fz . ,line) item))
                   (if extra `(,fz ,outline-id ,line) fz)))
               (gethash id db))))
          (gethash `(id ,id) org-roam-fztl--db))))

(defun org-roam-fztl--db-init ()
  "Fill mapping storage from all outline nodes."
  (org-roam-fztl--db-clear)
  (dolist (node (org-roam-fztl-node-outline-list))
    (let* ((buffer (find-file-noselect (org-roam-node-file node))))
      (with-current-buffer buffer
        (org-roam-fztl--db-from-outline)))))

(defun org-roam-fztl--db-init-maybe ()
  "If mapping storage is empty, initialize."
  (when (org-roam-fztl--db-empty-p)
    (org-roam-fztl--db-init)))

;;; TODO(2026-02-05): Improve the algorithm by performing update on affected
;;; tree.
(defun org-roam-fztl--db-from-outline ()
  "TBD."
  (let* ((node (org-roam-node-at-point))
         (outline-id (org-roam-node-id node))
         (start (string-to-number
                 (or (cdr (assoc "FZTL_START" (org-roam-node-properties node)))
                     "0")))
         (fz `(,start))
         (fdb (make-hash-table :test #'equal)))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elmt)
        (let ((level (org-element-property :level elmt))
              (line (line-number-at-pos (org-element-property :begin elmt)))
              vs)
          (setq fz (org-roam-fztl-fz--resize fz level))
          (org-roam-fztl-fz--lsd-inc fz)
          (when-let*
              ((lnk (org-element-map (org-element-property :title elmt) 'link
                      #'identity nil 'first-match))
               (id (and (equal (org-element-property :type lnk) "id")
                        (org-element-property :path lnk))))
            (setq vs (gethash id fdb))
            (push (cons (copy-sequence fz) line) vs)
            (puthash id vs fdb)

            (let* ((key `(id ,id))
                   (outline-ids (gethash key org-roam-fztl--db)))
              (cl-pushnew outline-id outline-ids :test #'equal)
              (puthash key outline-ids org-roam-fztl--db))

            (puthash `(fz ,(copy-sequence fz)) (cons id outline-id)
                     org-roam-fztl--db)))))
    (puthash `(outline ,outline-id) fdb org-roam-fztl--db)))

;;; Overlay Management

(defface org-roam-fztl-overlay
  `((t :inherit fixed-pitch
       :foreground ,(face-attribute 'shadow :foreground)
       :background ,(face-attribute 'shadow :background)))
  "Face used for fztl overlays.")

(defcustom org-roam-fztl-overlay-text-placement 'before-string
  "Specify side of overlay to show rendered folgezettel."
  :type '(choice before-string after-string)
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
  (when-let* ((fzs (org-roam-fztl--db-id2fz-get id)))
    (string-join (mapcar (lambda (fz)
                           (format org-roam-fztl-overlay-fz-format
                                   (org-roam-fztl-fz--render fz)))
                         fzs)
                 "")))

(defun org-roam-fztl-overlay--render-in-title ()
  "Render folgezettel overlays in document title.
The title is obtained from `#+title:'."
  (goto-char (point-min))     ; this respects narrowing
  (when-let* ((node (org-roam-node-at-point))
              (ovt (org-roam-fztl-overlay--format (org-roam-node-id node))))
    (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" (point-max) t)
      (org-roam-fztl-overlay--put (match-beginning 1) (match-end 1) ovt))))

(defun org-roam-fztl-overlay--render-in-headlines ()
  "Render folgezettel overlays in headlines.
IDs are extracted from headline properties."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (elmt)
      (when-let* ((id (org-element-property :ID elmt))
                  (ovt (org-roam-fztl-overlay--format id))
                  (beg (org-element-property :title-begin elmt))
                  (end (org-element-property :title-end elmt)))
        (org-roam-fztl-overlay--put beg end ovt)))))

(defun org-roam-fztl-overlay--render-link (elmt)
  "Render folgezettel ID overlay for link ELMT."
  (when-let* ((id (and (string= (org-element-property :type elmt) "id")
                       (org-element-property :path elmt)))
              (ovt (org-roam-fztl-overlay--format id))
              (beg (org-element-property :begin elmt))
              (end (org-element-property :end elmt)))
    (remove-overlays beg end 'category 'fztl)
    (org-roam-fztl-overlay--put beg end ovt)))

;; (defun org-roam-fztl-overlay--render-links ()
;;   "Render folgezettel ID overlays for all Org links found."
;;   (org-element-map (org-element-parse-buffer) 'link
;;     #'org-roam-fztl-overlay--render-link))

(defun org-roam-fztl-overlay--remove (&optional beg end buffer)
  "Remove folgezettel overlays in region from BEG to END of BUFFER."
  (org-roam-fztl--narrow-and-eval beg end buffer
    (remove-overlays beg end 'category 'fztl)))

(defun org-roam-fztl-overlay--render (&optional beg end buffer)
  "Render folgezettel overlays in region from BEG to END of BUFFER."
  (org-roam-fztl--narrow-and-eval beg end buffer
    (org-roam-fztl-overlay--render-in-title)
    (org-roam-fztl-overlay--render-in-headlines)
    (org-element-map (org-element-parse-buffer) 'link
      #'org-roam-fztl-overlay--render-link)))

(defun org-roam-fztl-overlay--refresh (&optional beg end buffer)
  "Refresh folgezettel overlays in region from BEG to END of BUFFER."
  (org-roam-fztl-overlay--remove beg end buffer)
  (org-roam-fztl-overlay--render beg end buffer))

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
  "Return non-nil if NODE is folgezettel outline.
A folgezettel outline is defined as an Org document with one of its file tags
being `org-roam-fztl-outline-tag'."
  (when-let* ((node (or node (org-roam-node-at-point)))
              (tags (org-roam-node-tags node)))
    (member org-roam-fztl-outline-tag tags)))

(defun org-roam-fztl-node-outline-list (&optional fun)
  "Get folgezettel outline nodes.
FUN is a function that takes an outline node as an argument and returns
identify (default) or its transformation."
  (let ((fun (or fun #'identity)))
    (seq-keep (lambda (node)
                (when (org-roam-fztl-node-outline-p node)
                  (funcall fun node)))
              (org-roam-node-list))))

(defun org-roam-fztl-node-outline-select ()
  "Select outline node interactively."
  (when-let*
      ((options (org-roam-fztl-node-outline-list
                 (lambda (node)
                   (cons (org-roam-node-title node) node)))))
    (alist-get (completing-read "Folgezettel outline: " options nil t)
               options nil nil #'equal)))

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
  "Name of indirect buffer visiting outline node file.")

(defun org-roam-fztl-outline-window--get ()
  "Return outline window if it is in use."
  (seq-find (lambda (win)
              (when-let* ((buf (window-buffer win)))
                (with-current-buffer buf
                  (derived-mode-p 'org-roam-fztl-outline-mode))))
            (window-list)))

(defun org-roam-fztl-outline-window--font-lock-sync (beg end)
  "Fontify indirect buffer from BEG to END."
  (when (and (derived-mode-p 'org-roam-fztl-outline-mode)
             (> (- end beg) 1)
             (buffer-base-buffer)
             (buffer-live-p (buffer-base-buffer))
             (text-property-any beg end 'fontified nil (current-buffer)))
    (font-lock-flush beg end)
    (font-lock-ensure beg end)))

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
      (unless (derived-mode-p 'org-roam-fztl-outline-mode)
        (org-roam-fztl-outline-mode)))
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

(defun org-roam-fztl-outline-window--display-indirect-buffer (node)
  "Display indirect buffer of NODE."
  (let* ((name (format "%s<%s>"
                       org-roam-fztl-outline-window--buffer-name
                       (org-roam-node-id node)))
         (buf (or (get-buffer name)
                  (make-indirect-buffer
                   (find-file-noselect (org-roam-node-file node))
                   name))))
    (org-roam-fztl-outline-window--display-buffer buf)))

(defun org-roam-fztl-outline-window--side (side)
  "Layout outline window to SIDE."
  (when-let* ((win (org-roam-fztl-outline-window--get)))
    (let ((buf (window-buffer win))
          (target-pos (and (eq win (selected-window))
                           (point))))
      (delete-window win)
      (setq win (org-roam-fztl-outline-window--display-buffer buf side))
      (when target-pos
        (select-window win)
        (goto-char target-pos)
        (org-reveal)
        (recenter)))))

(defun org-roam-fztl-outline-window-top ()
  "Layout outline window to top."
  (interactive)
  (org-roam-fztl-outline-window--side 'top))

(defun org-roam-fztl-outline-window-right ()
  "Layout outline window to right."
  (interactive)
  (org-roam-fztl-outline-window--side 'right))

(defun org-roam-fztl-outline-window-bottom ()
  "Layout outline window to bottom."
  (interactive)
  (org-roam-fztl-outline-window--side 'bottom))

(defun org-roam-fztl-outline-window-left ()
  "Layout outline window to left."
  (interactive)
  (org-roam-fztl-outline-window--side 'left))

;;;###autoload
(defun org-roam-fztl-outline-window-focus ()
  "Put focus on outline window.
If the current node is a folgezettel, the point will be on the entry in the
outline."
  (interactive)
  (org-roam-fztl-outline-window-on 'focus))

(defun org-roam-fztl-outline-window-on (&optional focus)
  "Show outline window.
If FOCUS is non-nil and the current node is a folgezettel, the point will be on
the entry in the outline."
  (let ((win (org-roam-fztl-outline-window--get))
        target-pos)
    (when-let*
        ((node (and (derived-mode-p 'org-mode) (org-roam-node-at-point)))
         (id (and node (org-roam-node-id node)))
         (items (and id (org-roam-fztl-fz--from-id id 'extra))))
      (let ((item (seq-find
                   (lambda (item)
                     (pcase-let*
                         ((`(,fz ,outline-id ,pos) item)
                          (outline-node (org-roam-node-from-id outline-id))
                          (file (org-roam-node-file outline-node)))
                       (when (and win
                                  (eq (buffer-base-buffer (window-buffer win))
                                      (find-buffer-visiting file)))
                         (setq target-pos pos)
                         t)))
                   items)))
        (unless item
          ;; No window displaying matching indirect buffer exists.
          (pcase-let* ((`(,fz ,outline-id ,pos) (car items))
                       (outline-node (org-roam-node-from-id outline-id)))
            (when win
              (delete-window win))
            (setq win (org-roam-fztl-outline-window--display-indirect-buffer
                       outline-node)
                  target-pos pos)))))
    (unless win
      (setq win (org-roam-fztl-outline-window--display-indirect-buffer
                 (org-roam-fztl-node-outline-select))))
    (when target-pos
      (with-selected-window win
        (goto-line target-pos)
        (org-reveal 'siblings)
        (recenter)))
    (when (and focus win)
      (select-window win 'norecord))))

(defun org-roam-fztl-outline-window-off ()
  "Hide outline window.
Returns non-nil if an outline window exists and is deleted."
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

(defun org-roam-fztl-outline--headline-link ()
  "Get link on headline at point if exists."
  (when-let*
      ((raw (org-get-heading t t t t))
       (parsed (org-element-parse-secondary-string raw '(link))))
    (org-element-map parsed 'link #'identity nil 'first-match)))

(defun org-roam-fztl-outline--headline-linked-node ()
  "Get node linked on headline at point if exists."
  (when-let*
      ((lnk (org-roam-fztl-outline--headline-link))
       (id (and (equal (org-element-property :type lnk) "id")
                (org-element-property :path lnk)))
       (node (org-roam-node-from-id id)))
    node))

(defun org-roam-fztl-outline-tags-refresh ()
  "Refresh tags in headline at point in outline node.
Use `org-roam-fztl-outline-tags-exclude' to exclude tags from being added."
  (interactive)
  (when (not (org-at-heading-p))
    (warn "Point not on Org headline"))
  (when-let*
      ((lnk (org-roam-fztl-outline--headline-link))
       (lnk-type (and lnk (org-element-property :type lnk)))
       (lnk-path (and lnk (org-element-property :path lnk)))
       (id (and (equal lnk-type "id") lnk-path))
       (node (org-roam-node-from-id id))
       (tags (cl-set-difference (org-roam-node-tags node)
                                org-roam-fztl-outline-tags-exclude
                                :test #'equal)))
    (org-set-tags tags)))

(defun org-roam-fztl-outline-tags-refresh-all ()
  "Refresh tags in all headlines in outline node.
On each headline, refresh is performed by `org-roam-fztl-outline-tags-refresh'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries #'org-roam-fztl-outline-tags-refresh)))

(defun org-roam-fztl-outline-preview-async ()
  "Async-open node linked in current headline."
  (when (org-at-heading-p)
    (org-roam-fztl--debounce 0.2
      (when-let* ((node (org-roam-fztl-outline--headline-linked-node)))
        (display-buffer (find-file-noselect (org-roam-node-file node))
                        '((display-buffer-use-some-window)
                          (inhibit-same-window . t)))))))

(defun org-roam-fztl-outline-preview-toggle ()
  "Toggle outline preview."
  (interactive)
  (when (org-roam-fztl-node-outline-p)
    (if (member #'org-roam-fztl-outline-preview-async post-command-hook)
        (remove-hook 'post-command-hook #'org-roam-fztl-outline-preview-async t)
      (add-hook 'post-command-hook #'org-roam-fztl-outline-preview-async nil t))))

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

(defun org-roam-fztl-outline-show-title ()
  "Show note title in target specified in `org-roam-fztl-outline-show-title'."
  (org-roam-fztl--debounce org-roam-fztl-outline-show-title-delay
    (when-let* ((lnk (org-roam-fztl-outline--headline-link))
                (contents (org-element-contents lnk))
                (desc (org-element-interpret-data contents)))
      (when (eq org-roam-fztl-outline-show-title 'minibuffer)
        (minibuffer-message "Note: %s" desc)))))

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
  (when-let* ((node (org-roam-fztl-node-outline-select))
              (win (org-roam-fztl-outline-window--get))
              (buf (window-buffer win)))
    (delete-window win)
    (kill-buffer buf)
    (setq win (org-roam-fztl-outline-window--display-indirect-buffer node))
    (select-window win 'norecord)))

(defun org-roam-fztl-outline-edit ()
  "Edit outline node in base buffer."
  (interactive)
  (when-let* ((node (org-roam-node-at-point)))
    (org-roam-node-visit node)))

(defun org-roam-fztl-outline--capture-active-p ()
  "Non-nil if at least one org-capture buffer is live (pre-finalize)."
  (seq-some (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'org-mode)
                     (bound-and-true-p org-capture-mode))))
            (buffer-list)))

(defmacro org-roam-fztl-outline--modify (&rest body)
  "Temporarily toggle `read-only-mode' while running BODY."
  `(let* ((buf (current-buffer))
          (base-buf (buffer-base-buffer))
          (inhibit-read-only t))
     (read-only-mode -1)
     (unwind-protect
         (condition-case err
             (progn ,@body)
           (quit              ; quit from `completing-read'
            (with-current-buffer base-buf
              (revert-buffer nil t)))
           (user-error        ; abort capture before creating node
            (with-current-buffer base-buf
              (revert-buffer nil t)))
           (error
            (with-current-buffer base-buf
              (revert-buffer nil t))
            (signal (car err) (cdr err))))

       ;; Do not save buffer if a capture is in session, in which case
       ;; save-or-revert is handled in the capture's after-finalize hook.
       (unless (org-roam-fztl-outline--capture-active-p)
         (with-current-buffer base-buf
           (when (buffer-modified-p)
             (save-buffer))))

       (with-current-buffer buf
         (read-only-mode +1)))))

(defmacro org-roam-fztl-outline--refresh-subtree (&rest body)
  `(let ((buf (current-buffer))
         reg-beg reg-end)
     (with-current-buffer buf
       (save-excursion
         (org-mark-subtree 1)
         (setq reg-beg (region-beginning) reg-end (region-end))
         (deactivate-mark))
       (org-roam-fztl-overlay--refresh reg-beg reg-end))
     ,@body
     (with-current-buffer buf
       (org-roam-fztl-overlay--refresh reg-beg reg-end))))

(defun org-roam-fztl-outline-insert-child ()
  "Insert child of current headline."
  (interactive)
  (org-roam-fztl-outline--modify
   (let ((org-insert-heading-respect-content t))
     (org-insert-heading))
   (org-do-demote)
   (org-roam-fztl--db-from-outline)
   (org-roam-fztl-outline--refresh-subtree
    (org-roam-node-insert)
    (org-roam-fztl--db-from-outline))))

(defun org-roam-fztl-outline-insert-sibling ()
  "Insert sibling of current headline."
  (interactive)
  (org-roam-fztl-outline--modify
   (org-insert-heading-respect-content)
   (org-roam-fztl--db-from-outline)
   (org-roam-fztl-outline--refresh-subtree
    (org-roam-node-insert)
    (org-roam-fztl--db-from-outline))))

(defun org-roam-fztl-outline-delete-subtree ()
  "Delete subtree of current headline."
  (interactive)
  (org-roam-fztl-outline--modify
   (beginning-of-line)
   (org-roam-fztl-outline--refresh-subtree
    (org-mark-subtree)
    (kill-region (region-beginning) (region-end))
    (deactivate-mark))
   (org-roam-fztl--db-from-outline)))

(defun org-roam-fztl-outline-refresh-tags ()
  "Refresh tags."
  (interactive)
  (org-roam-fztl-outline--modify
   (org-roam-fztl-outline-tags-refresh)))

(defvar-keymap org-roam-fztl-outline-mode-map
  :doc "Keymap for `org-roam-fztl-mode' under prefix."
  ;; Similar to org-speed-command:
  "n" #'org-next-visible-heading
  "p" #'org-previous-visible-heading
  "f" #'org-forward-heading-same-level
  "b" #'org-backward-heading-same-level
  "u" #'outline-up-heading

  "i" #'imenu
  "o" #'org-roam-fztl-outline-node-open
  "v" #'org-roam-fztl-outline-org-return
  "<return>" #'org-roam-fztl-outline-org-return
  "<backtab>" #'org-shifttab
  "<tab>" #'org-cycle

  "C" #'org-roam-fztl-outline-insert-child
  "S" #'org-roam-fztl-outline-insert-sibling
  "D" #'org-roam-fztl-outline-delete-subtree
  "E" #'org-roam-fztl-outline-edit
  "T" #'org-roam-fztl-outline-refresh-tags

  "O" #'org-roam-fztl-outline-switch-node
  "R" #'font-lock-fontify-buffer
  "V" #'org-roam-fztl-outline-preview-toggle

  "W t" #'org-roam-fztl-outline-window-top
  "W r" #'org-roam-fztl-outline-window-right
  "W b" #'org-roam-fztl-outline-window-bottom
  "W l" #'org-roam-fztl-outline-window-left)

(set-keymap-parent org-roam-fztl-outline-mode-map text-mode-map)

;;;###autoload
(define-derived-mode org-roam-fztl-outline-mode org-mode "fztl"
  "Major mode for folgezettel outline mode."
  :group 'org-roam
  ;; Load directory local variables, as indirect buffers do not load them by
  ;; default.
  (when-let*
      ((base (buffer-base-buffer))
       (default-directory (buffer-local-value 'default-directory base)))
    (hack-dir-local-variables-non-file-buffer))

  (read-only-mode 1)

  ;; Disable input method
  (make-local-variable 'current-input-method)
  (setq current-input-method nil)
  (make-local-variable 'default-input-method)
  (setq default-input-method nil)
  (org-roam-fztl--disable-command 'org-roam-fztl-outline-mode
                                  #'toggle-input-method)

  ;; Narrow to contents.
  (widen)
  (goto-char (point-min))
  (when (re-search-forward org-outline-regexp-bol nil t)
    (narrow-to-region (point-at-bol) (point-max))))

(defun org-roam-fztl-outline-mode--on-capture-before-finalize ()
  (when-let*
      ((buff (and (bound-and-true-p org-capture-plist)
                  (eq (plist-get org-capture-plist :finalize) 'insert-link)
                  (plist-get org-capture-plist :original-buffer))))
    (with-current-buffer buff
      (when (derived-mode-p 'org-roam-fztl-outline-mode)
        (read-only-mode -1)))))

(defun org-roam-fztl-outline-mode--on-capture-after-finalize ()
  (when-let* ((buf (buffer-base-buffer)))
    (with-current-buffer buf
      (if org-note-abort
          (revert-buffer nil t)
        (when (buffer-modified-p)
          (save-buffer)))
      (when (derived-mode-p 'org-roam-fztl-outline-mode)
        (read-only-mode +1)))))

(defun org-roam-fztl-outline-mode--on-after-change (beg end len)
  (when (> (- end beg) 1)
    (org-roam-fztl-outline-window--font-lock-sync beg end)))

(defun org-roam-fztl-outline-mode--on-window-scroll (win beg)
  (let ((end (window-end win t)))
    (org-roam-fztl-outline-window--font-lock-sync beg end)))

(defun org-roam-fztl-outline-mode--on-window-state-change (win)
  (org-roam-fztl-outline-window--font-lock-sync (window-start) (window-end)))

(defun org-roam-fztl-outline-mode--on-post-node-insert (id desc)
  (org-roam-fztl-outline-refresh-tags))

(defun org-roam-fztl-outline-mode--on-window-buffer-change (win)
  (let ((beg (window-start win))
        (end (window-end win t)))
    (org-roam-fztl-outline-window--font-lock-sync beg end)))

(defun org-roam-fztl-outline-mode--setup-hooks ()
  "Set up hooks for `org-roam-fztl-outline-mode'."
  (add-hook 'org-capture-before-finalize-hook
            #'org-roam-fztl-outline-mode--on-capture-before-finalize)
  (add-hook 'org-capture-after-finalize-hook
            #'org-roam-fztl-outline-mode--on-capture-after-finalize)
  (add-hook 'after-change-functions
            #'org-roam-fztl-outline-mode--on-after-change
            nil t)
  (add-hook 'window-scroll-functions
            #'org-roam-fztl-outline-mode--on-window-scroll
            nil t)
  (add-hook 'window-state-change-functions
            #'org-roam-fztl-outline-mode--on-window-state-change
            nil t)
  (add-hook 'post-command-hook
            #'org-roam-fztl-outline-modified--change-fringe
            nil t)
  (add-hook 'org-roam-post-node-insert-hook
            #'org-roam-fztl-outline-mode--on-post-node-insert
            99 t)
  (add-hook 'post-command-hook #'org-roam-fztl-outline-show-title
            nil t)
  (add-hook 'window-buffer-change-functions
            #'org-roam-fztl-outline-mode--on-window-buffer-change
            99 t))

(add-hook 'org-roam-fztl-outline-mode-hook
          #'org-roam-fztl-outline-mode--setup-hooks)

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

(defun org-roam-fztl-mode--on-window-scroll (win beg)
  (org-roam-fztl-overlay--refresh beg (window-end win t) (window-buffer win)))

(defun org-roam-fztl-mode--on-before-change (beg end)
  (org-roam-fztl-overlay--remove beg end))

(defun org-roam-fztl-mode--on-after-change (beg end len)
  (org-roam-fztl-overlay--render beg end))

(defun org-roam-fztl-mode--on-after-save ()
  (org-roam-fztl--db-from-outline)
  (when-let* ((win (org-roam-fztl-outline-window--get))
              (beg (window-start win))
              (end (window-end win t)))
    (with-selected-window win
      (org-roam-fztl-overlay--refresh beg end (window-buffer win))
      (org-roam-fztl-outline-window--font-lock-sync beg end))))

(defun org-roam-fztl-mode--on ()
  "Activate `org-roam-fztl-mode'."
  (add-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--db-init-maybe)
  (when (org-roam-fztl-node-outline-p)
    (add-hook 'after-save-hook #'org-roam-fztl-mode--on-after-save 99 t))
  (add-hook 'window-scroll-functions #'org-roam-fztl-mode--on-window-scroll 99 t)
  (add-hook 'before-change-functions #'org-roam-fztl-mode--on-before-change 99 t)
  (add-hook 'after-change-functions #'org-roam-fztl-mode--on-after-change 99 t))

(defun org-roam-fztl-mode--off ()
  "Deactivate `org-roam-fztl-mode'."
  (remove-hook 'after-change-functions #'org-roam-fztl-mode--on-after-change t)
  (remove-hook 'before-change-functions #'org-roam-fztl-mode--on-before-change t)
  (remove-hook 'window-scroll-functions #'org-roam-fztl-mode--on-window-scroll t)
  (when (org-roam-fztl-node-outline-p)
    (remove-hook 'after-save-hook #'org-roam-fztl-mode--on-after-save t))
  (remove-hook 'org-roam-fztl-mode-hook #'org-roam-fztl--db-init-maybe))

;;;###autoload
(define-minor-mode org-roam-fztl-mode
  "Minor mode for folgezettel support in Org Roam."
  :lighter " fztl"
  :group 'org-roam
  :keymap 'org-roam-fztl-mode-map
  (if org-roam-fztl-mode (org-roam-fztl-mode--on) (org-roam-fztl-mode--off)))

(provide 'org-roam-fztl)
;;; org-roam-fztl.el ends here
