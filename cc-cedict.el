;;; cc-cedict.el --- Interface to CC-CEDICT (a Chinese-English dictionary)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/cc-cedict.el
;; Created: 2018-12-03
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

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

;; cc-cedict.el is an Emacs interface for CC-CEDICT, a public-domain
;; Chinese-English dictionary.

;;; Code:

;; Download it from https://cc-cedict.org/wiki/
;;
;; $ wget https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.txt.gz
;; $ gunzip cedict_1_0_ts_utf-8_mdbg.txt.gz
(defvar cc-cedict-file (let ((file
                              (expand-file-name
                               "cedict_1_0_ts_utf-8_mdbg.txt"
                               (file-name-directory
                                (or load-file-name buffer-file-name)))))
                         (and (file-exists-p file) file))
  "Path to the dictionary file.")

(defun cc-cedict-parse ()
  (let (vec (idx 0))
    (with-temp-buffer
      (insert-file-contents cc-cedict-file)
      (goto-char (point-min))
      (re-search-forward "^[^#]")
      (goto-char (line-beginning-position))
      (setq vec (make-vector (count-lines (point) (point-max)) nil))
      (while (not (eobp))
        (if (looking-at (rx bol
                            (group (1+ (not (in " "))))
                            " "
                            (group (1+ (not (in " "))))
                            " "
                            "[" (group (1+ nonl)) "]"
                            " "
                            "/" (group (+ nonl)) "/"
                            eol))
            (aset vec idx (list :Traditional (match-string 1)
                                :Simplified (match-string 2)
                                :Pinyin (match-string 3)
                                :English (split-string (match-string 4) "/")))
          (error "Cannot parse '%s'"
                 (buffer-substring
                  (line-beginning-position) (line-end-position))))
        (setq idx (1+ idx))
        (forward-line 1))
      vec)))

(defvar cc-cedict-cache
  (and nil
       ;; I believe the vector is sorted alphabetically by `:Traditional'
       [(:Traditional "%" :Simplified "%" :Pinyin "pa1" :English ("percent (Tw)"))
        (:Traditional "21三體綜合症" :Simplified "21三体综合症" :Pinyin "er4 shi2 yi1 san1 ti3 zong1 he2 zheng4" :English ("trisomy" "Down's syndrome"))]))

(defun cc-cedict-completing-read ()
  (unless cc-cedict-cache
    (setq cc-cedict-cache (cc-cedict-parse)))
  (completing-read "Chinese: "
                   (mapcar (lambda (plist)
                             (plist-get plist :Simplified))
                           cc-cedict-cache)))

;;;###autoload
(defun cc-cedict (chinese)
  (interactive (list (cc-cedict-completing-read)))
  (unless cc-cedict-cache
    (setq cc-cedict-cache (cc-cedict-parse)))
  (let ((idx 0)
        (len (length cc-cedict-cache))
        (plist nil))
    (while (and (< idx len) (not plist))
      (let ((pl (aref cc-cedict-cache idx)))
        (when (or (string= (plist-get pl :Traditional) chinese)
                  (string= (plist-get pl :Simplified) chinese))
          (setq plist pl)))
      (setq idx (1+ idx)))
    (when (called-interactively-p 'interactive)
      ;; TODO Prettify the display, here is the official look
      ;; https://cc-cedict.org/editor/editor.php?handler=QueryDictionary&querydictionary_search=%E5%A7%8A%E5%A6%B9
      (message "%S" plist))
    plist))

(provide 'cc-cedict)
;;; cc-cedict.el ends here
