;;; or-struktur.el --- Structure Notes Plugin for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/or-struktur
;; Version: 0.19.2
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
;; This Org Roam plugin provides support for structure notes (strukturzettel in
;; German).
;;
;;; Code:

(require 'org)
(require 'org-roam)

(defgroup or-struktur nil
  "Settings for `or-struktur'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/or-struktur"))

(defcustom or-struktur-mode-prefix "C-c f"
  "Prefix key sequence for `or-struktur-mode' commands."
  :type 'string
  :group 'or-struktur)

(defcustom or-struktur-sz-tag "sz"
  "Tag for `org-roam' nodes indicating strukturzettels."
  :type 'string
  :group 'or-struktur)

(defcustom or-struktur-sid-text-placement 'before-string
  "Specify side of overlay to show rendered SID."
  :type '(choice before-string after-string)
  :group 'or-struktur)

(defcustom or-struktur-sid-text-wrapper "[%s]"
  "String wrapper used for text representation of SID.
When set to nil, text wrapping is disabled for all SIDs, regardless of
per-strukturzettel settings."
  :type 'string
  :group 'or-struktur)

(defcustom or-struktur-sid-text-format '("%d" ".%d")
  "String format for SID representation.
Allowed formatters are '%d' (numeric) and '%s' (alphabetic). When extended,
alphanumeric components are alternated.

Examples:

  - (\"%d\" \"%s\") will render like '2d3a1' (numeric then alphabet, with the
    rest alternating).
  - (\"%d\" \"-%s\") will render like '2-d3a1' (second digit preceded by a
    hyphen, with the rest alternating)."
  :type '(list string)
  :group 'or-struktur)

(defcustom or-struktur-view-layout '(top . 0.167)
  "Side window layout options."
  :type '(alist :key-type (choice (const :tag "Top" top)
                                  (const :tag "Right" right)
                                  (const :tag "Left" left)
                                  (const :tag "Bottom" bottom))
                :value-type number)
  :group 'or-struktur)

(defcustom or-struktur-view-tags-exclude nil
  "Tags to exclude from being added to headlines in strukturzettels."
  :type '(repeat string)
  :group 'or-struktur)

(defcustom or-struktur-view-show-title 'minibuffer
  "Target to show node title at point.
Either nil or `minibuffer' is allowed."
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Do not show" nil))
  :group 'or-struktur)

(defcustom or-struktur-view-show-title-delay 0.2
  "Delay before showing title in outline."
  :type 'number
  :group 'or-struktur)

(defface or-struktur-overlay
  `((t :inherit fixed-pitch
       :height 0.85
       :underline nil
       :foreground ,(face-attribute 'shadow :foreground)
       :background ,(face-attribute 'shadow :background)))
  "Face used for SID overlays.")

(defface or-struktur-view-modified
  '((((background dark))  :inherit region)
    (((background light)) :inherit region))
  "Fringe face for modified strukturzettel buffer.")

(defvar or-struktur--db (make-hash-table :test #'equal)
  "Mapping storage.")

(defvar or-struktur-view--layout-size-default 0.167)

(defvar or-struktur--ov-faces nil)

(defvar or-struktur-sid-text-wrapper--alist nil)

;;; Utilities

(defmacro or-struktur--debounce (delay &rest body)
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

(defmacro or-struktur--disable-command (mode command)
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

(defmacro or-struktur--narrow-and-eval (beg end buffer &rest body)
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

;;;
;;; Minor Mode (or-struktur-mode)
;;;

(defvar-keymap or-struktur-mode-prefix-map
  :doc "Keymap for `or-struktur-mode' under prefix."
  "a" #'or-struktur-node-find
  "c" #'or-struktur-node-find-children
  "p" #'or-struktur-node-find-parents
  "s" #'or-struktur-node-find-siblings
  "i c" #'or-struktur-node-insert-child
  "i p" #'or-struktur-node-insert-parent
  "i s" #'or-struktur-node-insert-sibling)

(defvar-keymap or-struktur-mode-map
  :doc "Keymap for `or-struktur-mode'.")

(keymap-set or-struktur-mode-map or-struktur-mode-prefix or-struktur-mode-prefix-map)

;;;###autoload
(define-minor-mode or-struktur-mode
  "Minor mode for strukturzettels support to Org Roam."
  :lighter " struct"
  :group 'org-roam
  :keymap 'or-struktur-mode-map
  (pcase or-struktur-mode
    ('t (or-struktur-mode--on))
    (_ (or-struktur-mode--off))))

(defun or-struktur-mode--on ()
  "Activate `or-struktur-mode'."
  (add-hook 'or-struktur-mode-hook #'or-struktur--db-init-maybe)
  (when (or-struktur-sz-p)
    (add-hook 'after-save-hook #'or-struktur-mode--on-after-save 99 t))
  (add-hook 'window-scroll-functions #'or-struktur-mode--on-window-scroll 99 t)
  (add-hook 'before-change-functions #'or-struktur-mode--on-before-change 99 t)
  (add-hook 'after-change-functions #'or-struktur-mode--on-after-change 99 t))

(defun or-struktur-mode--off ()
  "Deactivate `or-struktur-mode'."
  (remove-hook 'after-change-functions #'or-struktur-mode--on-after-change t)
  (remove-hook 'before-change-functions #'or-struktur-mode--on-before-change t)
  (remove-hook 'window-scroll-functions #'or-struktur-mode--on-window-scroll t)
  (when (or-struktur-sz-p)
    (remove-hook 'after-save-hook #'or-struktur-mode--on-after-save t))
  (remove-hook 'or-struktur-mode-hook #'or-struktur--db-init-maybe))

(defun or-struktur-mode--on-before-change (beg end)
  (or-struktur--ov-remove beg end))

(defun or-struktur-mode--on-after-change (beg end len)
  (or-struktur--ov-render beg end))

(defun or-struktur-mode--on-window-scroll (win beg)
  (or-struktur--ov-refresh beg (window-end win t) (window-buffer win)))

(defun or-struktur-mode--on-after-save ()
  (or-struktur--db-from-strukturzettel)
  (when-let* ((win (or-struktur-view--get-window))
              (beg (window-start win))
              (end (window-end win t)))
    (with-selected-window win
      (or-struktur--ov-refresh beg end (window-buffer win))
      (or-struktur-view--font-lock-sync beg end))))

;;; Mapping Storage for ID-SID Relations

;; NOTE: The storage implementation uses a hash table. For scalability, perhaps
;; consider using a relational database, e.g., SQLite.

(defun or-struktur--db-empty-p ()
  "Return non-nil if mapping storage is empty."
  (= (hash-table-count or-struktur--db) 0))

(defun or-struktur--db-clear ()
  "Empty mapping storage."
  (clrhash or-struktur--db))

(defun or-struktur--db-sid2id-get (sid)
  "Get ID for SID from mapping storage."
  (when-let* ((v (gethash `(sid ,sid) or-struktur--db)))
    (car v)))

(defun or-struktur--db-id2sid-get (id &optional extra)
  "Get all SIDs associated with ID from mapping storage.
When EXTRA is non-nil, return also strukturzettel ID and the position of SID
entry."
  (apply #'append
         (seq-keep
          (lambda (sz-id)
            (when-let*
                ((db (gethash `(sz ,sz-id)
                              or-struktur--db)))
              (mapcar
               (lambda (item)
                 (pcase-let* ((`(,sid . ,line) item))
                   (if extra `(,sid ,sz-id ,line) sid)))
               (gethash id db))))
          (gethash `(id ,id) or-struktur--db))))

(defun or-struktur--db-init ()
  "Fill mapping storage from all known strukturzettels."
  (or-struktur--db-clear)
  (dolist (node (or-struktur-sz-list))
    (let* ((buffer (find-file-noselect (org-roam-node-file node))))
      (with-current-buffer buffer
        (or-struktur--db-from-strukturzettel)))))

(defun or-struktur--db-init-maybe ()
  "If mapping storage is empty, initialize."
  (when (or-struktur--db-empty-p)
    (or-struktur--db-init)))

(defun or-struktur--prop-get (props key &optional default)
  "Get value for KEY in node PROPS.
If value is nil, returns DEFAULT."
  (let ((key (concat "STRUKTUR_" key)))
    (or (cdr (assoc key props)) default)))

(defun or-struktur--conf-from-props (start props)
  "Configure style for SID starting from START from node properties PROPS."
  (when-let* ((v (or-struktur--prop-get props "TEXT_WRAPPER")))
    (setf (alist-get start or-struktur-sid-text-wrapper--alist) v)
    (setq or-struktur-sid-text-wrapper--alist (sort or-struktur-sid-text-wrapper--alist)))
  (let (plist box)
    (when-let* ((v (or-struktur--prop-get props "FACE_FOREGROUND")))
      (setq plist (append plist `(:foreground ,v))))
    (when-let* ((v (or-struktur--prop-get props "FACE_BACKGROUND")))
      (setq plist (append plist `(:background ,v))))
    (when-let* ((v (or-struktur--prop-get props "FACE_BOX_LINE_WIDTH")))
      (setq box (append box `(:line-width ,v))))
    (when-let* ((v (or-struktur--prop-get props "FACE_BOX_COLOR")))
      (setq box (append box `(:color
                              ,(pcase v
                                 ("nil" (face-background 'default nil t))
                                 (_ v))))))
    (when box
      (setq plist (append plist `(:box ,box))))
    (when plist
      (setf (alist-get start or-struktur--ov-faces) plist)
      (setq or-struktur--ov-faces (sort or-struktur--ov-faces)))))

;; TODO(2026-02-05): Improve by performing update on affected tree.
(defun or-struktur--db-from-strukturzettel ()
  "Update storage mapping from current strukturzettel."
  (let* ((node (org-roam-node-at-point))
         (sz-id (org-roam-node-id node))
         (props (org-roam-node-properties node))
         (start (string-to-number (or-struktur--prop-get props "START" "0")))
         (sid `(,start))
         (fdb (make-hash-table :test #'equal)))
    (or-struktur--conf-from-props start props)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elmt)
        (let ((level (org-element-property :level elmt))
              (line (line-number-at-pos (org-element-property :begin elmt) t))
              vs)
          (setq sid (or-struktur-sid--resize sid level))
          (or-struktur-sid--lsd-inc sid)
          (when-let*
              ((lnk (org-element-map (org-element-property :title elmt) 'link
                      #'identity nil 'first-match))
               (id (and (equal (org-element-property :type lnk) "id")
                        (org-element-property :path lnk))))
            (setq vs (gethash id fdb))
            (push (cons (copy-sequence sid) line) vs)
            (puthash id vs fdb)

            (let* ((key `(id ,id))
                   (sz-ids (gethash key or-struktur--db)))
              (cl-pushnew sz-id sz-ids :test #'equal)
              (puthash key sz-ids or-struktur--db))

            (puthash `(sid ,(copy-sequence sid)) (cons id sz-id)
                     or-struktur--db)))))
    (puthash `(sz ,sz-id) fdb or-struktur--db)))

;;; Overlay Management

(defun or-struktur--binary-search-floor (alist key)
  "Return ALIST element whose key is floor of KEY."
  (let* ((left 0)
         (right (1- (length alist)))
         result)
    (while (<= left right)
      (let* ((mid (+ left (/ (- right left) 2)))
             (entry (nth mid alist))
             (val (car entry)))
        (cond
         ((= val key) (setq result entry) (setq left (1+ mid)))
         ((< val key) (setq result entry) (setq left (1+ mid)))
         (t (setq right (1- mid))))))
    result))

(defun or-struktur--ov-put (beg end text)
  "Add TEXT overlay for content from BEG to END."
  (let* ((ov (make-overlay beg end))
         (space (propertize " "
                            'face '( :inherit or-struktur-overlay
                                     :height 0.25 )))
         (text (pcase or-struktur-sid-text-placement
                 ('before-string (concat text space))
                 ('after-string (concat space text)))))
    (overlay-put ov or-struktur-sid-text-placement text)
    (overlay-put ov 'category 'or-struktur)
    (overlay-put ov 'evaporate t)))

(defun or-struktur--ov-format (id)
  "Render SIDs for ID as string for overlay."
  (when-let* ((sids (or-struktur--db-id2sid-get id)))
    (string-join
     (mapcar
      (lambda (sid)
        (let ((text-wrapper
               (if or-struktur-sid-text-wrapper
                   (or (cdr (or-struktur--binary-search-floor
                             or-struktur-sid-text-wrapper--alist (car sid)))
                       or-struktur-sid-text-wrapper)
                 "%s"))
              (face (append '(:inherit or-struktur-overlay)
                            (cdr (or-struktur--binary-search-floor
                                  or-struktur--ov-faces (car sid))))))
          (propertize (format text-wrapper (or-struktur-sid--render sid))
                      'face face)))
      sids)
     (string ?\u200B))))

(defun or-struktur--ov-render-in-title ()
  "Render SID overlays in document title.
The title is obtained from `#+title:'."
  (goto-char (point-min))     ; this respects narrowing
  (when-let* ((node (org-roam-node-at-point))
              (s (or-struktur--ov-format (org-roam-node-id node))))
    (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" (point-max) t)
      (or-struktur--ov-put (match-beginning 1) (match-end 1) s))))

(defun or-struktur--ov-render-in-headlines ()
  "Render SID overlays in headlines.
IDs are extracted from headline properties."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (elmt)
      (when-let* ((id (org-element-property :ID elmt))
                  (s (or-struktur--ov-format id))
                  (beg (org-element-property :title-begin elmt))
                  (end (org-element-property :title-end elmt)))
        (or-struktur--ov-put beg end s)))))

(defun or-struktur--ov-render-link (elmt)
  "Render SID overlay for link ELMT."
  (when-let* ((id (and (string= (org-element-property :type elmt) "id")
                       (org-element-property :path elmt)))
              (s (or-struktur--ov-format id))
              (beg (org-element-property :begin elmt))
              (end (org-element-property :end elmt)))
    (or-struktur--ov-put beg end s)))

(defun or-struktur--ov-remove (&optional beg end buffer)
  "Remove SID overlays in BUFFER region from BEG to END."
  (or-struktur--narrow-and-eval beg end buffer
    (remove-overlays beg end 'category 'or-struktur)))

(defun or-struktur--ov-render (&optional beg end buffer)
  "Render SID overlays in BUFFER region from BEG to END."
  (or-struktur--narrow-and-eval beg end buffer
    (or-struktur--ov-render-in-title)
    (or-struktur--ov-render-in-headlines)
    (org-element-map (org-element-parse-buffer) 'link
      #'or-struktur--ov-render-link)))

(defun or-struktur--ov-refresh (&optional beg end buffer)
  "Refresh SID overlays in BUFFER region from BEG to END."
  (or-struktur--ov-remove beg end buffer)
  (or-struktur--ov-render beg end buffer))

;;; Struktur ID (SID)

;; An SID is an ID of an org-roam node assigned by its placement in a
;; strukturzettel. The underlying data for SID is a list of integers, e.g., (2 3
;; 5).

(defun or-struktur-sid--number-to-alpha (n)
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

(defun or-struktur-sid--render (sid)
  "Render SID using preset format.
The preset format is set with `or-struktur-sid-text-format'."
  (string-join
   (cl-loop with (converter) = '(nil)
            for i below (length sid)
            collect
            (let ((fmt (or (nth i or-struktur-sid-text-format)
                           (if (eq converter #'identity) "%s" "%d"))))
              (setq converter (or (and (string-search "%d" fmt) #'identity)
                                  #'or-struktur-sid--number-to-alpha))
              (format fmt (funcall converter (nth i sid)))))))

(defun or-struktur-sid--resize (sid n &optional initval)
  "Resize SID to N digits.
If given, fill new digit(s) with INITVAL (defaults to zero)."
  (let ((initval (or initval 0))
        (m (length sid)))
    (cond ((< n m) (seq-take sid n))
          ((< m n) (append sid (make-list (- n m) initval)))
          (t sid))))

(defun or-struktur-sid--lsd-inc (sid)
  "Increment least-significant digit of SID."
  (let ((lsd (1- (length sid))))
    (setcar (nthcdr lsd sid) (1+ (nth lsd sid)))))

(defun or-struktur-sid--from-id (&optional id extra)
  "Get SIDs for node ID.
If not given, ID defaults to that of current node.

See `or-struktur--mapping-id2fz-get' for EXTRA."
  (or-struktur--db-id2sid-get (or id (org-roam-id-at-point)) extra))

(defun or-struktur-sid--to-id (sid)
  "Get node ID for SID."
  (or-struktur--db-sid2id-get sid))

(defun or-struktur-sid--get-children (sid)
  "Get SIDs of existing child nodes for SID."
  (let (result)
    (if sid
        (let* ((sid (copy-sequence sid))
               (fz-child (or-struktur-sid--resize sid (1+ (length sid)) 1)))
          (while (or-struktur--db-sid2id-get fz-child)
            (push (copy-sequence fz-child) result)
            (or-struktur-sid--lsd-inc fz-child)))
      ;; Top-level folgezettels could be non-contiguous.
      (maphash (lambda (k _)
                 (pcase-let ((`(,type ,sid) k))
                   (when (and (eq type 'sid) (= (length sid) 1))
                     (push (copy-sequence sid) result))))
               or-struktur--db))
    result))

(defun or-struktur-sid--get-parents (sid)
  "Get parent SID of SID."
  (list (butlast sid)))

(defun or-struktur-sid--get-siblings (sid)
  "Get SIDs of existing siblings for SID."
  (apply #'append
         (mapcar (lambda (sid)
                   (or-struktur-sid--get-children sid))
                 (or-struktur-sid--get-parents sid))))

;;; Strukturzettel Nodes

(defun or-struktur-sz-p (&optional node)
  "Return non-nil if NODE is strukturzettel node.
A strukturzettel is defined as an Org document with one of its file tags being
`or-struktur-sz-tag'."
  (when-let* ((node (or node (org-roam-node-at-point)))
              (tags (org-roam-node-tags node)))
    (member or-struktur-sz-tag tags)))

(defun or-struktur-sz-list (&optional fun)
  "Get strukturzettel nodes.
FUN (default: `identity') is a function that takes a strukturzettel node as an
argument and returns transformation."
  (let ((fun (or fun #'identity)))
    (seq-keep (lambda (node)
                (when (or-struktur-sz-p node)
                  (funcall fun node)))
              (org-roam-node-list))))

(defun or-struktur-sz-select ()
  "Select strukturzettel node.
The function calls `completing-read' to prompt for a node interactively."
  (when-let*
      ((options (or-struktur-sz-list
                 (lambda (node)
                   (cons (org-roam-node-title node) node)))))
    (alist-get (completing-read "Strukturzettel: " options nil t)
               options nil nil #'equal)))

;;; Nodes

(defun or-struktur-node-has-sid-p (&optional node)
  "Return non-nil if NODE has SID.
If not given, NODE will be node at point."
  (and (or-struktur-sid--from-id (and node (org-roam-node-id node))) t))

(defun or-struktur-node-find ()
  "Find and open node found in any strukturzettel."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (or-struktur-node-has-sid-p node))))

(defun or-struktur-node--op (type filter-fn)
  "Perform operation of TYPE on nodes related to node at point.
TYPE is either `find' or `insert'.

The function FILTER-FN takes an SID and returns related nodes."
  (if-let* ((node (and (derived-mode-p 'org-mode) (org-roam-node-at-point)))
            (id (and (or-struktur-node-has-sid-p node)
                     (org-roam-node-id node))))
      (if-let* ((ids (seq-keep
                      (lambda (sid) (or-struktur-sid--to-id sid))
                      (apply #'append
                             (seq-keep
                              (lambda (sid) (funcall filter-fn sid))
                              (or-struktur-sid--from-id id)))))
                (fun (lambda (node) (member (org-roam-node-id node) ids))))
          (pcase type
            ('find (org-roam-node-find nil nil fun))
            ('insert (org-roam-node-insert fun)))
        (message "No nodes found"))
    (warn "Not in a relevant org-roam node")))

(defun or-struktur-node-find-children ()
  "Find and open children of node at point."
  (interactive)
  (or-struktur-node--op 'find #'or-struktur-sid--get-children))

(defun or-struktur-node-find-parents ()
  "Find and open parents of node at point."
  (interactive)
  (or-struktur-node--op 'find #'or-struktur-sid--get-parents))

(defun or-struktur-node-find-siblings ()
  "Find and open siblings of node at point."
  (interactive)
  (or-struktur-node--op 'find #'or-struktur-sid--get-siblings))

(defun or-struktur-node-insert-child ()
  "Insert Org link to child of node at point."
  (interactive)
  (or-struktur-node--op 'insert #'or-struktur-sid--get-children))

(defun or-struktur-node-insert-parent ()
  "Insert Org link to parent of node at point."
  (interactive)
  (or-struktur-node--op 'insert #'or-struktur-sid--get-parents))

(defun or-struktur-node-insert-sibling ()
  "Insert Org link to sibling of node at point."
  (interactive)
  (or-struktur-node--op 'insert #'or-struktur-sid--get-siblings))

;;;
;;; Major Mode (or-struktur-view-mode)
;;;

(defvar-keymap or-struktur-view-mode-map
  :doc "Keymap for `or-struktur-mode' under prefix."
  ;; Similar to org-speed-command:
  "n" #'org-next-visible-heading
  "p" #'org-previous-visible-heading
  "f" #'org-forward-heading-same-level
  "b" #'org-backward-heading-same-level
  "u" #'outline-up-heading

  "i" #'imenu
  "o" #'or-struktur-view-node-open
  "v" #'or-struktur-view-org-return
  "<return>" #'or-struktur-view-org-return
  "<backtab>" #'org-shifttab
  "<tab>" #'org-cycle

  "C" #'or-struktur-view-insert-child
  "S" #'or-struktur-view-insert-sibling
  "D" #'or-struktur-view-delete-subtree
  "T" #'or-struktur-view-edit-link-desc
  "E" #'or-struktur-view-edit

  "O" #'or-struktur-view-switch-node
  "R" #'font-lock-fontify-buffer
  "V" #'or-struktur-view-preview-toggle

  "W t" #'or-struktur-view-top
  "W r" #'or-struktur-view-right
  "W b" #'or-struktur-view-bottom
  "W l" #'or-struktur-view-left)

(set-keymap-parent or-struktur-view-mode-map text-mode-map)

;;;###autoload
(define-derived-mode or-struktur-view-mode org-mode "struktur"
  "Major mode for strukturzettels."
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
  (or-struktur--disable-command 'or-struktur-view-mode
                                #'toggle-input-method)

  ;; Narrow to contents.
  (widen)
  (goto-char (point-min))
  (when (re-search-forward org-outline-regexp-bol nil t)
    (narrow-to-region (point-at-bol) (point-max))))

(defun or-struktur-view--on-capture-before-finalize ()
  (when-let*
      ((buff (and (bound-and-true-p org-capture-plist)
                  (eq (plist-get org-capture-plist :finalize) 'insert-link)
                  (plist-get org-capture-plist :original-buffer))))
    (with-current-buffer buff
      (when (derived-mode-p 'or-struktur-view-mode)
        (read-only-mode -1)))))

(defun or-struktur-view--on-capture-after-finalize ()
  (when-let* ((buf (buffer-base-buffer)))
    (with-current-buffer buf
      (if org-note-abort
          (revert-buffer nil t)
        (when (buffer-modified-p)
          (save-buffer)))
      (when (derived-mode-p 'or-struktur-view-mode)
        (read-only-mode +1)))))

(defun or-struktur-view--on-after-change (beg end len)
  (when (> (- end beg) 1)
    (or-struktur-view--font-lock-sync beg end)))

(defun or-struktur-view--on-window-scroll (win beg)
  (let ((end (window-end win t)))
    (or-struktur-view--font-lock-sync beg end)))

(defun or-struktur-view--on-window-state-change (win)
  (or-struktur-view--font-lock-sync (window-start) (window-end)))

(defun or-struktur-view--on-post-node-insert (id desc)
  (or-struktur-view-refresh-tags))

(defun or-struktur-view--on-window-buffer-change (win)
  (let ((beg (window-start win))
        (end (window-end win t)))
    (or-struktur-view--font-lock-sync beg end)))

(defun or-struktur-view--on-setup ()
  "Set up hooks for `or-struktur-view-mode'."
  (add-hook 'org-capture-before-finalize-hook
            #'or-struktur-view--on-capture-before-finalize)
  (add-hook 'org-capture-after-finalize-hook
            #'or-struktur-view--on-capture-after-finalize)
  (add-hook 'after-change-functions
            #'or-struktur-view--on-after-change nil t)
  (add-hook 'window-scroll-functions
            #'or-struktur-view--on-window-scroll nil t)
  (add-hook 'window-state-change-functions
            #'or-struktur-view--on-window-state-change nil t)
  (add-hook 'post-command-hook
            #'or-struktur-view-modified--change-fringe nil t)
  (add-hook 'org-roam-post-node-insert-hook
            #'or-struktur-view--on-post-node-insert 99 t)
  (add-hook 'post-command-hook
            #'or-struktur-view-show-title nil t)
  (add-hook 'window-buffer-change-functions
            #'or-struktur-view--on-window-buffer-change 99 t))

(add-hook 'or-struktur-view-mode-hook #'or-struktur-view--on-setup)

(defconst or-struktur-view-window--buffer-name " outline buffer"
  "Name of indirect buffer visiting outline node file.")

(defun or-struktur-view--link ()
  "Return first Org link element on current line or nil if it does not exist."
  (save-excursion
    (beginning-of-line)
    (let ((end (line-end-position))
          lnk)
      (while (and (not lnk)
                  (< (point) end)
                  (re-search-forward org-link-any-re end t))
        (goto-char (match-beginning 0))
        (when-let ((el (org-element-context)))
          (when (eq (org-element-type el) 'link)
            (setq lnk el))))
      lnk)))

(defun or-struktur-view--headline-link ()
  "Get link on headline at point if exists."
  (when-let*
      ((raw (org-get-heading t t t t))
       (parsed (org-element-parse-secondary-string raw '(link))))
    (org-element-map parsed 'link #'identity nil 'first-match)))

(defun or-struktur-view--headline-linked-node ()
  "Get node linked on headline at point if exists."
  (when-let*
      ((lnk (or-struktur-view--headline-link))
       (id (and (equal (org-element-property :type lnk) "id")
                (org-element-property :path lnk)))
       (node (org-roam-node-from-id id)))
    node))

(defun or-struktur-view-tags-refresh ()
  "Refresh tags in headline at point in outline node.
Use `or-struktur-view-tags-exclude' to exclude tags from being added."
  (interactive)
  (when (not (org-at-heading-p))
    (warn "Point not on Org headline"))
  (when-let*
      ((lnk (or-struktur-view--headline-link))
       (lnk-type (and lnk (org-element-property :type lnk)))
       (lnk-path (and lnk (org-element-property :path lnk)))
       (id (and (equal lnk-type "id") lnk-path))
       (node (org-roam-node-from-id id))
       (tags (cl-set-difference (org-roam-node-tags node)
                                or-struktur-view-tags-exclude
                                :test #'equal)))
    (org-set-tags tags)))

(defun or-struktur-view-tags-refresh-all ()
  "Refresh tags in all headlines in outline node.
On each headline, refresh is performed by `or-struktur-view-tags-refresh'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries #'or-struktur-view-tags-refresh)))

(defun or-struktur-view-preview-async ()
  "Async-open node linked in current headline."
  (when (org-at-heading-p)
    (or-struktur--debounce 0.2
      (when-let* ((node (or-struktur-view--headline-linked-node)))
        (display-buffer (find-file-noselect (org-roam-node-file node))
                        '((display-buffer-use-some-window)
                          (inhibit-same-window . t)))))))

(defun or-struktur-view-preview-toggle ()
  "Toggle outline preview."
  (interactive)
  (when (or-struktur-sz-p)
    (if (member #'or-struktur-view-preview-async post-command-hook)
        (remove-hook 'post-command-hook #'or-struktur-view-preview-async t)
      (add-hook 'post-command-hook #'or-struktur-view-preview-async nil t))))

;; Show node title in minibuffer

(defun or-struktur-view-show-title ()
  "Show note title in target specified in `or-struktur-view-show-title'."
  (or-struktur--debounce or-struktur-view-show-title-delay
    (when-let* ((lnk (or-struktur-view--headline-link))
                (contents (org-element-contents lnk))
                (desc (org-element-interpret-data contents)))
      (when (eq or-struktur-view-show-title 'minibuffer)
        (minibuffer-message "Note: %s" desc)))))

;; When outline buffer is modified, indicate with fringe color.

(defvar-local or-struktur-view-modified--cookie nil)

(defun or-struktur-view-modified--change-fringe ()
  "Indicate when outline buffer is modified.
Add this to `post-command-hook'."
  (if (buffer-modified-p)
      (setq-local or-struktur-view-modified--cookie
                  (face-remap-add-relative
                   'fringe 'or-struktur-view-modified))
    (when or-struktur-view-modified--cookie
      (face-remap-remove-relative or-struktur-view-modified--cookie)
      (set-window-fringes (selected-window) nil nil t)
      (setq-local or-struktur-view-modified--cookie nil))))

(defun or-struktur-view-org-return (&rest _rest)
  "Override `org-return' for faster navigation.
This command changes default behavior to find a link on current header and visit
if such a link exists."
  (interactive)
  (if-let* ((node (or-struktur-view--headline-linked-node)))
      (org-roam-node-visit node)
    (apply #'org-return _rest)))

(defun or-struktur-view-node-open ()
  "Open node at point in another window."
  (interactive)
  (when-let*
      ((node (or-struktur-view--headline-linked-node))
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

(defun or-struktur-view-switch-node ()
  "Switch to different outline node."
  (interactive)
  (when-let* ((node (or-struktur-sz-select))
              (win (or-struktur-view--get-window))
              (buf (window-buffer win)))
    (delete-window win)
    (kill-buffer buf)
    (setq win (or-struktur-view--display-indirect-buffer node))
    (select-window win 'norecord)))

(defun or-struktur-view-edit ()
  "Edit outline node in base buffer."
  (interactive)
  (when-let*
      ((pos (point))
       (node (org-roam-node-at-point))
       (buf (progn (org-roam-node-visit node)
                   (get-file-buffer (org-roam-node-file node)))))
    (with-current-buffer buf
      (goto-char pos)
      (org-fold-show-context)
      (recenter))))

(defun or-struktur-view--capture-active-p ()
  "Non-nil if at least one org-capture buffer is live (pre-finalize)."
  (seq-some (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'org-mode)
                     (bound-and-true-p org-capture-mode))))
            (buffer-list)))

(defmacro or-struktur-view--modify (&rest body)
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
       (unless (or-struktur-view--capture-active-p)
         (with-current-buffer base-buf
           (when (buffer-modified-p)
             (save-buffer))))

       (with-current-buffer buf
         (read-only-mode +1)))))

(defmacro or-struktur-view--refresh-subtree (&rest body)
  `(let ((buf (current-buffer))
         reg-beg reg-end)
     (with-current-buffer buf
       (save-excursion
         (org-mark-subtree 1)
         (setq reg-beg (region-beginning) reg-end (region-end))
         (deactivate-mark))
       (or-struktur--ov-refresh reg-beg reg-end))
     ,@body
     (with-current-buffer buf
       (or-struktur--ov-refresh reg-beg reg-end))))

(defun or-struktur-view-insert-child ()
  "Insert child of current headline."
  (interactive)
  (or-struktur-view--modify
   (let ((org-insert-heading-respect-content t))
     (org-insert-heading))
   (org-do-demote)
   (or-struktur--db-from-strukturzettel)
   (or-struktur-view--refresh-subtree
    (org-roam-node-insert)
    (or-struktur--db-from-strukturzettel))))

(defun or-struktur-view-insert-sibling ()
  "Insert sibling of current headline."
  (interactive)
  (or-struktur-view--modify
   (let ((org-insert-heading-respect-content t))
     (org-insert-heading))
   (or-struktur--db-from-strukturzettel)
   (or-struktur-view--refresh-subtree
    (org-roam-node-insert)
    (or-struktur--db-from-strukturzettel))))

(defun or-struktur-view-delete-subtree ()
  "Delete subtree of current headline."
  (interactive)
  (or-struktur-view--modify
   (beginning-of-line)
   (or-struktur-view--refresh-subtree
    (org-mark-subtree)
    (kill-region (region-beginning) (region-end))
    (deactivate-mark))
   (or-struktur--db-from-strukturzettel)))

(defun or-struktur-view-edit-link-desc ()
  "Edit link description."
  (interactive)
  (or-struktur-view--modify
   (when-let*
       ((lnk (or-struktur-view--link))
        (type (org-element-property :type lnk))
        (id (and (equal type "id") (org-element-property :path lnk)))
        (beg (org-element-property :begin lnk))
        (end (org-element-property :end lnk))
        (desc (buffer-substring-no-properties
               (org-element-property :contents-begin lnk)
               (org-element-property :contents-end lnk)))
        (node (org-roam-node-from-id id))
        (titles (cons (org-roam-node-title node) (org-roam-node-aliases node)))
        (desc (completing-read "Change description: " titles nil nil desc)))
     (goto-char beg)
     (delete-region beg end)
     (insert (format "[[%s:%s][%s]]" type id desc)))
   (or-struktur-view-tags-refresh)))

(defun or-struktur-view-refresh-tags ()
  "Refresh tags."
  (interactive)
  (or-struktur-view--modify
   (or-struktur-view-tags-refresh)))

;;; Strukturzettel Window Management

;;;###autoload
(defun or-struktur-view-focus ()
  "Put focus on strukturzettel view.
If the current buffer is a strukturzettel, the point will be on its entry in the
strukturzettel."
  (interactive)
  (or-struktur-view--show 'focus))

;;;###autoload
(defun or-struktur-view-toggle ()
  "Toggle strukturzettel view."
  (interactive)
  (unless (or-struktur-view--hide)
    (or-struktur-view--show)))

(defun or-struktur-view-top ()
  "Layout strukturzettel view to top."
  (interactive)
  (or-struktur-view--layout-side 'top))

(defun or-struktur-view-right ()
  "Layout strukturzettel view to right."
  (interactive)
  (or-struktur-view--layout-side 'right))

(defun or-struktur-view-bottom ()
  "Layout strukturzettel view to bottom."
  (interactive)
  (or-struktur-view--layout-side 'bottom))

(defun or-struktur-view-left ()
  "Layout strukturzettel view to left."
  (interactive)
  (or-struktur-view--layout-side 'left))

(defun or-struktur-view--layout-current (&optional side)
  "Set layout to SIDE or get currently active layout when SIDE is not given.
When not given, SIDE defaults to the first entry in `or-struktur-view-layout'."
  ;; Ensure list of cons.
  (setq or-struktur-view-layout
        (cond ((null or-struktur-view-layout) nil)
              ((consp (car or-struktur-view-layout))
               or-struktur-view-layout)
              ((consp or-struktur-view-layout)
               (list or-struktur-view-layout))
              (t nil)))
  (if side
      (setq or-struktur-view-layout
            (if-let* ((v (assq side or-struktur-view-layout)))
                (cons v (delq v or-struktur-view-layout))
              (cons (cons side or-struktur-view--layout-size-default)
                    or-struktur-view-layout))))
  (car or-struktur-view-layout))

(defun or-struktur-view--layout-side (side)
  "Layout strukturzettel window to SIDE."
  (when-let* ((win (or-struktur-view--get-window)))
    (let ((buf (window-buffer win))
          (target-pos (and (eq win (selected-window))
                           (point))))
      (delete-window win)
      (setq win (or-struktur-view--display-buffer buf side))
      (when target-pos
        (select-window win)
        (goto-char target-pos)
        (org-reveal)
        (recenter)))))

(defun or-struktur-view--get-window ()
  "Return strukturzettel window in use or nil."
  (seq-find (lambda (win)
              (when-let* ((buf (window-buffer win)))
                (with-current-buffer buf
                  (derived-mode-p 'or-struktur-view-mode))))
            (window-list)))

(defun or-struktur-view--font-lock-sync (beg end)
  "Fontify currently selected indirect buffer from BEG to END."
  (when (and (derived-mode-p 'or-struktur-view-mode)
             (> (- end beg) 1)
             (buffer-base-buffer)
             (buffer-live-p (buffer-base-buffer))
             (text-property-any beg end 'fontified nil (current-buffer)))
    (font-lock-flush beg end)
    (font-lock-ensure beg end)))

(defun or-struktur-view--display-buffer (buffer &optional side)
  "Display strukturzettel BUFFER in SIDE window.
This function returns the newly created side window."
  (pcase-let*
      ((`(,side . ,size)
        (or-struktur-view--layout-current side))
       (window-size
        (cons (if (member side '(top bottom))
                  #'window-height #'window-width)
              size)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'or-struktur-view-mode)
        (or-struktur-view-mode)))
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

(defun or-struktur-view--display-indirect-buffer (node)
  "Display indirect buffer of NODE."
  (let* ((name (format "%s<%s>"
                       or-struktur-view-window--buffer-name
                       (org-roam-node-id node)))
         (buf (or (get-buffer name)
                  (make-indirect-buffer
                   (find-file-noselect (org-roam-node-file node))
                   name))))
    (or-struktur-view--display-buffer buf)))

(defun or-struktur-view--show (&optional focus)
  "Show strukturzettel window.
If FOCUS is non-nil and the current node is associated with a strukturzettel,
the point will be on the entry in the strukturzettel."
  (let ((win (or-struktur-view--get-window))
        target-pos)
    (when-let*
        ((node (and (derived-mode-p 'org-mode) (org-roam-node-at-point)))
         (id (and node (org-roam-node-id node)))
         (items (and id (or-struktur-sid--from-id id 'extra))))
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
            (setq win (or-struktur-view--display-indirect-buffer
                       outline-node)
                  target-pos pos)))))
    (unless win
      (if-let* ((node (or-struktur-sz-select)))
          (setq win (or-struktur-view--display-indirect-buffer
                     node))
        (error "No strukturzettels found")))
    (when target-pos
      (with-selected-window win
        (goto-line target-pos)
        (org-reveal 'siblings)
        (recenter)))
    (when (and focus win)
      (select-window win 'norecord))))

(defun or-struktur-view--hide ()
  "Hide strukturzettel window.
This function returns non-nil if a strukturzettel window exists and is deleted."
  (when-let* ((win (or-struktur-view--get-window)))
    (delete-window win)
    t))

(provide 'or-struktur)
;;; or-struktur.el ends here
