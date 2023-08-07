;;; dendroam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Rodriguez
;;
;; Author: Victor Rodriguez <https://github.com/vrodriguez>
;; Maintainer: Victor Rodriguez <vrodriguez@confluent.io>
;; Created: April 26, 2021
;; Modified: April 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/vrodriguez/dendroam
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'dendroam)

(defvar dendroam-capture-templates
  '(("t" "Time note" entry
     "* %?"
     :if-new (file+head "${current-file}.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n"))
    ("s" "Scratch note" entry
     "* %?"
     :if-new (file+head "scratch.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n")))

  "Some utils templates for different type of notes such us time notes
or sratch notes")

(defun dendroam-replace-dot-with-tilde (input-str)
  "逐一对字符串中的字符进行判断，如果句号处于数字之间，则将句号替换为~号。"
  (let ((output-str "")
        (len (length input-str))
        (i 0))
    (while (< i len)
      (let ((current-char (aref input-str i))
            (next-char (if (< (1+ i) len) (aref input-str (1+ i)) nil))
            (after-next-char (if (< (+ i 2) len) (aref input-str (+ i 2)) nil)))
        (if (and (>= current-char ?0) (<= current-char ?9)
                 (and next-char (char-equal next-char ?.)
                      (and after-next-char (>= after-next-char ?0) (<= after-next-char ?9))))
            (progn
              (setq output-str (concat output-str (char-to-string current-char) "~"))
              (setq i (1+ i))) ; Skip the next character (the dot)
          (setq output-str (concat output-str (char-to-string current-char))))
        (setq i (1+ i))))
    output-str))

(defun dendroam-replace-tilde-with-dot (input-str)
  "逐一对字符串中的字符进行判断，如果~号处于数字之间，则将句号替换为句号。"
  (let ((output-str "")
        (len (length input-str))
        (i 0))
    (while (< i len)
      (let ((current-char (aref input-str i))
            (next-char (if (< (1+ i) len) (aref input-str (1+ i)) nil))
            (after-next-char (if (< (+ i 2) len) (aref input-str (+ i 2)) nil)))
        (if (and (>= current-char ?0) (<= current-char ?9)
                 (and next-char (char-equal next-char ?~)
                      (and after-next-char (>= after-next-char ?0) (<= after-next-char ?9))))
            (progn
              (setq output-str (concat output-str (char-to-string current-char) "."))
              (setq i (1+ i))) ; Skip the next character (the tidle)
          (setq output-str (concat output-str (char-to-string current-char))))
        (setq i (1+ i))))
    output-str))

(defun dendroam-capitalize-title-words (title)
  "Capitalize the first word of the TITLE and words after '.', '?', ':', ';', '!'."
  (let* ((words (split-string title " "))
         (result '())
         (capitalize-next t))
    (dolist (word words)
      (when (not (string-empty-p word))
        (push (if capitalize-next
                  (progn
                    (setq capitalize-next nil)
                    (capitalize word))
                word)
              result)
        (when (string-match-p "[.?:;!]" (substring word -1))
          (setq capitalize-next t))))
    (setq result (nreverse result))
    (string-join result " ")))

(defun dendroam-url-encode-special-chars (string)
  "Encode special characters in STRING using URL encoding."
  (replace-regexp-in-string
   "[?:;! ]"
   (lambda (char)
     (format "%%%02x" (string-to-char char)))
   string))

;;Node custom getters
(cl-defmethod org-roam-node-current-file (node)
  "Gets node file-name-base by file name"
  (file-name-base (org-roam-node-file node)))

(cl-defmethod org-roam-node-hierarchy-title (node)
  "Gets node title excluding the hierarchy and capitalize it"
  (dendroam-replace-tilde-with-dot
   (dendroam-capitalize-title-words
    (car
     (last
      (split-string
       (dendroam-replace-dot-with-tilde
        (org-roam-node-title node)) "\\."))))))
;; (capitalize
;;  (car
;;   (last
;;    (split-string
;;     (org-roam-node-title node)
;;     "\\.")))))

(defun dendroam-format-hierarchy (file)
  "Formats node's path, to get the hierarchy whithout the title
where title will be the last child of the hierarchy:
from the filename this.is.a.hierarchy.note-title.org
returns this.is.a.hierarchy"
  (let* ((base-name (file-name-base file))
         (hierarchy-no-title
          (dendroam-replace-tilde-with-dot
           (file-name-base
            (dendroam-replace-dot-with-tilde base-name)))))
    hierarchy-no-title))

(cl-defmethod org-roam-node-hierarchy (node)
  "Gets node hierarchy by file name"
  (funcall 'dendroam-format-hierarchy (org-roam-node-file node)))

(cl-defmethod org-roam-node-current-file (node)
  (file-name-base (buffer-file-name)))

;; Refactor functions

(defun dendroam-fetch-same-hierarchy-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY totally or parcially"
  (let ((files
         (mapcar #'car (org-roam-db-query [:select [file]
                                                   :from nodes
                                                   :where (like file $r1)]
                                          (concat "%" hierarchy "%")))))
    files))

(defun dendroam-refactor-hierarchy (&optional current)
  "Prompts the user to change the hierarchy of the current file
node and updates its hierarchy and the hierarchy of all the nodes
that have it if CURRENT is t the list of updated files is just
the current file"
  (interactive)
  (let*
      ((initial-file
        (file-name-nondirectory (buffer-file-name)))
       (initial-slug
        (file-name-base initial-file))
       (new-slug (file-name-base
                  (read-string "Refactor: " initial-slug)))
       (initial-slug-no-title
        (file-name-base initial-slug))
       (files-to-upd (if current
                         `(,initial-file)
                       (dendroam-fetch-same-hierarchy-files
                        initial-slug-no-title))))

    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))

(defun dendroam-refactor-file ()
  (interactive)
  (let* ((initial-file (buffer-file-name))
         (initial-slug (url-unhex-string (file-name-base initial-file)))
         (initial-directory (file-name-directory (buffer-file-name)))
         (new-slug (read-string "Refactor: " initial-slug))
         (new-file (concat
                    (expand-file-name (dendroam-url-encode-special-chars new-slug) initial-directory)
                    ".org")))
    (message "new-file是: %s" new-file)
    (setq new-title (file-name-base
                     (replace-regexp-in-string
                      (concat
                       initial-directory
                       (dendroam-url-encode-special-chars (dendroam-format-hierarchy new-file))
                       ".") "" new-file)))
    (dendroam-update-org-title (dendroam-capitalize-title-words (url-unhex-string new-title)))
    (save-buffer)
    (rename-file initial-file  new-file)
    (kill-current-buffer)
    (find-file new-file)))

(defun dendroam-update-org-title (new-title)
  "Update the #+title in the current org file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:.*$" nil t)
      (replace-match (format "#+title: %s" new-title)))))

;; Useful notes functions
(defun dendroam-insert-time-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "t"
                     :props (list :default-time (current-time))))

(defun dendroam-insert-scratch-note(&optional goto)
  "Creates a time note in the current level of the hierarchy.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "s"
                     :props (list :default-time (current-time))))

;; Org roam overrides to allow these features
(eval-after-load "org-roam"
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
     "Return the slug of NODE."
     (let ((title (org-roam-node-title node))
           (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                              768 ; U+0300 COMBINING GRAVE ACCENT
                              769 ; U+0301 COMBINING ACUTE ACCENT
                              770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                              771 ; U+0303 COMBINING TILDE
                              772 ; U+0304 COMBINING MACRON
                              774 ; U+0306 COMBINING BREVE
                              775 ; U+0307 COMBINING DOT ABOVE
                              776 ; U+0308 COMBINING DIAERESIS
                              777 ; U+0309 COMBINING HOOK ABOVE
                              778 ; U+030A COMBINING RING ABOVE
                              779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                              780 ; U+030C COMBINING CARON
                              795 ; U+031B COMBINING HORN
                              803 ; U+0323 COMBINING DOT BELOW
                              804 ; U+0324 COMBINING DIAERESIS BELOW
                              805 ; U+0325 COMBINING RING BELOW
                              807 ; U+0327 COMBINING CEDILLA
                              813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                              814 ; U+032E COMBINING BREVE BELOW
                              816 ; U+0330 COMBINING TILDE BELOW
                              817 ; U+0331 COMBINING MACRON BELOW
                              )))
       (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                  (strip-nonspacing-marks (s) (string-glyph-compose
                                               (apply #'string
                                                      (seq-remove #'nonspacing-mark-p
                                                                  (string-glyph-decompose s)))))
                  (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
         (let* ((pairs `(("[^[:alnum:][:digit:].-?:;! ]" . "_") ;; convert anything not alphanumeric
                         ("__*" . "_")                   ;; remove sequential underscores
                         ("^_" . "")                     ;; remove starting underscore
                         ("_$" . "")                     ;; remove ending underscore
                         ("[?]" . "%3F")                 ;; replace "?" with its URL encoding "%3F"
                         ("[:]" . "%3A")                 ;; replace ":" with its URL encoding "%3A"
                         ("[;]" . "%3B")                 ;; replace ";" with its URL encoding "%3B"
                         ("[!]" . "%21")                 ;; replace "!" with its URL encoding "%21"
                         (" " . "%20")))                 ;; replace space with its URL encoding "%20"
                (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
           (downcase slug))))))



;;; dendroam.el ends here
