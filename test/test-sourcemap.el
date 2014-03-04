;;; test-sourcemap.el --- sourcemap.el test

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'sourcemap)

(defvar test-input
"{
  \"version\": 3,
  \"file\": \"min.js\",
  \"names\": [\"bar\", \"baz\", \"n\"],
  \"sources\": [\"one.js\", \"two.js\"],
  \"sourceRoot\": \"http://example.com/www/js/\",
  \"mappings\": \"CAAC,IAAI,IAAM,SAAUA,GAClB,OAAOC,IAAID;CCDb,IAAI,IAAM,SAAUE,GAClB,OAAOA\"
}")

(ert-deftest original-position-for ()
  "Get original position from line and column of generated file"
  (let* ((sourcemap (sourcemap-from-string test-input))
         (original (sourcemap-original-position-for sourcemap :line 2 :column 28)))
    (should (= (plist-get original :line) 2))
    (should (= (plist-get original :column) 10))))

(ert-deftest generated-position-for ()
  "Get position in generated file from line and column of source file"
  (let* ((sourcemap (sourcemap-from-string test-input))
         (generated (sourcemap-generated-position-for
                     sourcemap :source "two.js" :line 2 :column 10)))
    (should (= (plist-get generated :line) 2))
    (should (= (plist-get generated :column) 28))))

(provide 'test-sourcemap)

;;; test-sourcemap.el ends here
