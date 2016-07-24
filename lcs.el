;;; lcs.el --- Longest Common Subsequences           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ian Price

;; Author: Ian Price <ianprice90@gmail.com>
;; Keywords: TODO: ---------------------
;; Version: 0.1
;; Package-Requires: TODO: ((emacs "24") (cl-lib "0.5"))
;; URL: TODO:

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

;; This code is purposefully not generalised to handle different
;; sequences and tests. I'm open to it, but this code is designed for strings

;;; Code:
(require 'cl-lib)

(defun lcs--table-ref (table idx1 idx2)
  (aref (aref table idx1) idx2))

(defun lcs--table-set! (table idx1 idx2 obj) ; aset
  (setf (aref (aref table idx1) idx2) obj))

(defun lcs--make-table (size1 size2)
  (let ((table (make-vector size1 nil)))
    (cl-loop for i from 0 below size1
             do (aset table i (make-vector size2 0)))
    table))

(defun lcs--compute-table (s1 s2)
  (let* ((rows (1+ (length s1)))
         (cols (1+ (length s2)))
         (table (lcs--make-table rows cols)))
    (cl-loop
     for row from 1 below rows do
     (cl-loop
      for col from 1 below cols do
      (if (eql (aref s1 (1- row))
               (aref s2 (1- col)))
          (lcs--table-set! table
                           row
                           col
                           (1+ (lcs--table-ref table (1- row) (1- col))))
        (lcs--table-set! table
                         row
                         col
                         (max (lcs--table-ref table (1- row) col)
                              (lcs--table-ref table row (1- col)))))))
    table))

(defun lcs-size (s1 s2)
  (let ((table (lcs--compute-table s1 s2)))
    (lcs--table-ref table (1- (length s1)) (1- (length s2)))))

(defun lcs-substring (s1 s2)
  (let ((table (lcs--compute-table s1 s2))
        (x (length s1))
        (y (length s2))
        (out nil))
    (while (/= 0 (lcs--table-ref table x y))
      (cond ((= (lcs--table-ref table x y) (lcs--table-ref table (1- x) y))
             (setq x (1- x)))
            ((= (lcs--table-ref table x y) (lcs--table-ref table x (1- y)))
             (setq y (1- y)))
            (t
             (push (aref s1 (1- x)) out)
             (setq x (1- x)
                   y (1- y)))))
    (concat out)))

(defun lcs-diff (s1 s2)
  (let ((table (lcs--compute-table s1 s2))
        (x (length s1))
        (y (length s2))
        (out nil))
    (while (/= 0 (lcs--table-ref table x y))
      (let ((x-start x)
            (y-start y))
        (while (= (lcs--table-ref table x y) (lcs--table-ref table (1- x) y))
          (setq x (1- x)))
        (while (= (lcs--table-ref table x y) (lcs--table-ref table x (1- y)))
          (setq y (1- y)))
        (unless (and (= x x-start) (= y y-start))
          (push (cons (substring s1 x x-start)
                      (substring s2 y y-start))
                out)))
      (let ((x-start x)
            (y-start y))
        (while (not (or (zerop (lcs--table-ref table x y))
                        (= (lcs--table-ref table x y)
                           (lcs--table-ref table (1- x) y))
                        (= (lcs--table-ref table x y)
                           (lcs--table-ref table x (1- y)))))
          (setq x (1- x)
                y (1- y)))
        (unless (and (= x x-start) (= y y-start))
          (push (substring s1 x x-start) out))))
    out))

(provide 'lcs)
;;; lcs.el ends here
