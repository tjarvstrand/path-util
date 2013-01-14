;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of path-util.
;;
;; path-util is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; path-util is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with path-util. If not, see <http://www.gnu.org/licenses/>.
;;
;; Path handling utilities

(defun path-util-dir-name (path)
  "Return the file-name of PATH's directory (no trailing separator)."
  (directory-file-name (file-name-non-directory (path))))

(defun path-util-root-base-name (path)
  "Return the root-name (w/o extension) of the base-name (last
component) of PATH"
  (file-name-sans-extension (file-name-nondirectory path)))

(defun path-util-pop (path &optional count)
  "Pop COUNT levels from PATH. COUNT defaults to 1"
  (let ((count (or count 1)))
    (while (>= (setq count (1- count)) 0)
      (setq path (directory-file-name (file-name-directory path))))
    path))

(defun path-util-join (&rest paths)
  "Join strings in PATHS with a direcory separator in between each
element."
  (mapconcat #'identity paths "/"))

(provide 'path-util)
