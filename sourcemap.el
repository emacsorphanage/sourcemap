;;; sourcemap.el --- Sourcemap parser

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-sourcemap
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst sourcemap--vlq-shift-width 5)

(defconst sourcemap--vlq-mask (1- (ash 1 5))
  "0b011111")

(defconst sourcemap--vlq-continuation-bit (ash 1 5)
  "0b100000")

(defvar sourcemap--char2int-table (make-hash-table :test 'equal))

(let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
  (cl-loop for char across chars
           for index = 0 then (1+ index)
           do
           (progn
             (puthash (char-to-string char) index sourcemap--char2int-table))))

(defsubst sourcemap--continuation-value-p (value)
  (not (zerop (logand value sourcemap--vlq-continuation-bit))))

(defsubst sourcemap--value (value)
  (logand value sourcemap--vlq-mask))

(defun sourcemap--base64-decode (char-str)
  (let ((val (gethash char-str sourcemap--char2int-table 'not-found)))
    (if (eq val 'not-found)
        (error "Invalid input '%s'" char-str)
      val)))

(defun sourcemap--from-vlq-signed (value)
  (let ((negative-p (= (logand value 1) 1))
        (shifted (ash value -1)))
    (if negative-p
        (- shifted)
      shifted)))

(defun sourcemap-base64-vlq-decode (str)
  (let ((length (length str)))
    (cl-loop for char across str
             for index = 1 then (1+ index)
             for shift = 0 then (+ shift sourcemap--vlq-shift-width)
             for digit = (sourcemap--base64-decode (char-to-string char))
             for continue-p = (sourcemap--continuation-value-p digit)
             for value = (sourcemap--value digit)
             for result = value then (+ result (ash value shift))
             unless continue-p
             return (list :value (sourcemap--from-vlq-signed result)
                          :rest (substring str index)))))

(defun sourcemap--parse-group (group)
  (cl-loop with sections = (split-string group ",")
           for section in sections
           collect (sourcemap-base64-vlq-decode section)))

(defsubst sourcemap--starts-with-separator-p (str)
  (let ((char (substring str 0 1)))
    (or (string= char ",") (string= char ";"))))

(defun sourcemap--retrieve (sourcemap property)
  (let ((value (assoc-default property sourcemap)))
    (if (not value)
        (error "Can't found '%s' property" property)
      value)))

(defun sourcemap--sources-at (sourcemap index)
  (let ((sources (sourcemap--retrieve sourcemap 'sources)))
    (aref sources index)))

(defun sourcemap--names-at (sourcemap index)
  (let ((names (sourcemap--retrieve sourcemap 'names)))
    (aref names index)))

(defun sourcemap--parse-mappings (sourcemap)
  (let ((str (sourcemap--retrieve sourcemap 'mappings))
        (generated-line 1) (previous-generated-column 0)
        (previous-source 0) (previous-original-line 0)
        (previous-original-column 0) (previous-source 0) (previous-name 0)
        (mappings '())
        temp (count 0))
    (while (> (length str) 0)
      (cond ((string= (substring str 0 1) ";")
             (setq generated-line (1+ generated-line)
                   str (substring str 1)
                   previous-generated-column 0))
            ((string= (substring str 0 1) ",")
             (setq str (substring str 1)))
            (t
             (let ((mapping (list :generated-line generated-line)))
               ;; Generated Column
               (setq temp (sourcemap-base64-vlq-decode str))
               (let ((current-column (+ previous-generated-column (plist-get temp :value))))
                 (setq previous-generated-column current-column)
                 (plist-put mapping :generated-column current-column))

               ;; Original source
               (setq str (plist-get temp :rest))
               (when (and (> (length str) 0)
                          (not (sourcemap--starts-with-separator-p str)))
                 (setq temp (sourcemap-base64-vlq-decode str))
                 (let* ((source-value (plist-get temp :value))
                        (current-source (+ previous-source source-value)))
                   (plist-put mapping :source
                              (sourcemap--sources-at sourcemap current-source))
                   (cl-incf previous-source source-value))
                 (setq str (plist-get temp :rest))
                 (when (or (zerop (length str)) (sourcemap--starts-with-separator-p str))
                   (error "Found a source, but no line and column"))

                 ;; Original line
                 (setq temp (sourcemap-base64-vlq-decode str))
                 (let ((original-line (+ previous-original-line (plist-get temp :value))))
                   (setq previous-original-line original-line)
                   (plist-put mapping :original-line (1+ original-line))) ; 0-base
                 (setq str (plist-get temp :rest))

                 ;; Original column
                 (setq temp (sourcemap-base64-vlq-decode str))
                 (let ((original-column (+ previous-original-column (plist-get temp :value))))
                   (plist-put mapping :original-column original-column)
                   (setq previous-original-column original-column))
                 (setq str (plist-get temp :rest))
                 ;; Original name
                 (when (and (> (length str) 0)
                            (not (sourcemap--starts-with-separator-p str)))
                   (setq temp (sourcemap-base64-vlq-decode str))
                   (let* ((name-value (plist-get temp :value))
                          (name-index (+ previous-name name-value)))
                     (plist-put mapping
                                :name (sourcemap--names-at sourcemap name-index))
                     (cl-incf previous-name name-value))
                   (setq str (plist-get temp :rest))))
               (push mapping mappings)))))
    (reverse mappings)))

(defun sourcemap-generated-position-for (sourcemap &rest props)
  (let ((mappings (sourcemap--parse-mappings sourcemap))
        (original-source (plist-get props :source))
        (line (plist-get props :line))
        (column (plist-get props :column)))
    (cl-loop for map in mappings
             for source = (plist-get map :source)
             for original-line = (plist-get map :original-line)
             for original-column = (plist-get map :original-column)
             when (and (string= original-source source)
                       (= original-line line) (= original-column column))
             return (list :line (plist-get map :generated-line)
                          :column (plist-get map :generated-column)))))

(defun sourcemap-original-position-for (sourcemap &rest props)
  (let ((mappings (sourcemap--parse-mappings sourcemap))
        (line (plist-get props :line))
        (column (plist-get props :column)))
    (cl-loop for map in mappings
             for generated-line = (plist-get map :generated-line)
             for generated-column = (plist-get map :generated-column)
             when (and (= generated-line line) (= generated-column column))
             return (list :line (plist-get map :original-line)
                          :column (plist-get map :original-column)))))

;;;###autoload
(defun sourcemap-from-file (file)
  (interactive
   (list (read-file-name "Sourcemap File: " nil nil t)))
  (json-read-file file))

;;;###autoload
(defun sourcemap-from-string (str)
  (json-read-from-string str))

(provide 'sourcemap)

;;; sourcemap.el ends here
