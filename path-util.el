;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of path-util.
;;
;; path-util is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; path-util is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with path-util. If not, see <http://www.gnu.org/licenses/>.
;;
;; Path handling utilities

(require 'cl)

(defun path-util-file-in-dir-p (file dir)
  "Return non-nil of file is located in DIR or a subdirectory of DIR.
Unlike `file-in-directory-p', this function does not derefence symlinks
and does not require that FILE or DIRECTORY exist."
  (string-prefix-p (path-util-normalize dir)
                   (expand-file-name file)))

(defun path-util-normalize (path)
  "Expands PATH, replaces duplicate /'s and ensures that its formatted
as a directory-name."
  (file-name-as-directory
   (expand-file-name (replace-regexp-in-string "//+" "/" path))))


(defun path-util-dir-name (path)
  "Return the file-name of PATH's directory (ignores trailing
separators).

Example:
foo      -> nil
/foo     -> /
/foo/    -> /
/foo/bar -> bar"
  (let ((dir (file-name-directory (directory-file-name path))))
    (when dir
      (directory-file-name dir))))

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

(defun* path-util-join (&rest paths &key (expand nil) &allow-other-keys)
  "Join strings in PATHS with a direcory separator in between each
element.

If keyword :expand is non-nil, call `expand-file-name' on the resulting
path before returning."
  (when (setq paths (delq nil (path-util--remove-keyword-params paths)))
    (let* ((res (mapconcat #'identity paths "/")))
      (if expand
          (expand-file-name res)
        res))))

(defun path-util--remove-keyword-params (seq)
  (let ((res nil))
    (while seq
      (if (keywordp (car seq))
          (setq seq (cddr seq))
        (push (pop seq) res)))
    (nreverse res)))

;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(when (member 'ert features)

  (ert-deftest path-util-file-in-dir-p-test ()
    (should
     (not (null (path-util-file-in-dir-p "/foo" "/foo/bar/baz.el"))))
    (should
     (not (path-util-file-in-dir-p "/bar" "/foo/bar/baz.el"))))

  (ert-deftest path-util-normalize-test ()
    (let ((dir (directory-file-name default-directory)))
      (should (string= (path-util-join dir "foo/bar/")
                       (path-util-normalize "foo//bar")))))

  (ert-deftest path-util-dir-name-test ()
    (should-error (path-util-dir-name nil))
    (should-not   (path-util-dir-name ""))
    (should-not (path-util-dir-name "foo"))
    (should (string= "/" (path-util-dir-name "/foo")))
    (should (string= "/" (path-util-dir-name "/foo/")))
    (should (string= "/foo" (path-util-dir-name "/foo/bar")))
    (should (string= "/foo" (path-util-dir-name "/foo/bar/"))))

  (ert-deftest path-util-join-test ()
    (should-not (path-util-join nil))
    (should (string= "foo" (path-util-join "foo")))
    (should (string= "foo/bar" (path-util-join "foo" "bar")))
    (should (string= "foo/bar" (path-util-join "foo" "bar")))
    (should (string= "foo//bar" (path-util-join "foo/" "bar")))
    (should (string= (directory-file-name default-directory)
                     (path-util-join "" :expand t)))
    (should-not (path-util-join nil :expand t)))
    (should (string= (concat (expand-file-name "~") "/foo")
                     (path-util-join "~" "foo" :expand t))))

  (ert-deftest path-util--remove-keyword-params-test ()
    (should-not (path-util--remove-keyword-params nil))
    (should-not (path-util--remove-keyword-params '(:foo bar)))
    (should (equal '(a) (path-util--remove-keyword-params '(a :foo bar))))
    (should (equal '(a) (path-util--remove-keyword-params '(:foo bar a))))
    (should (equal '(a a) (path-util--remove-keyword-params '(a :foo bar a)))))

(provide 'path-util)
