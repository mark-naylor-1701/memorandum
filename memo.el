;;; memo.el --- Interactive creation of memoranda.

;; Copyright (©) 2017  Mark W. Naylor <mark.naylor.1701@gmail.com>

;; Author: Mark W. Naylor
;; Version: 0.9
;; Package-Requires: ((s "2017102.227"))
;; Keywords: office memorandum
;; Created:  2017-May-03

;;TODO:
;;; Assign suitable copyright notice. (Probably a BSD version).

(require 's)

(setq lexical-binding t)

(defun memo-generate ()
  (interactive)
  (find-file (read-string "memo file name:  " "memo.txt"))

  (insert (s-join "\n" (append (mapcar #'memo--formatted-item (memo--items)) '(""))))
  (newline 2)
  (insert "MESSAGE BODY"))

(defun memo--items-base ()
      (list
       (list "TO" (list "To" #'memo--recipient))
       (list "FROM" (list "From" #'user-full-name))
       (list "SUBJ" (list "Subject" #'memo--subject))
       (list "DATE" (list "Date" #'short-date))))

(defun memo--items ()
  (let ((items (memo--items-base)))
    (-zip
     (memo--paddify (mapcar #'memo--colon (mapcar #'car items)))
     (mapcar #'cadr items))))

(defun memo--formatted-item (memo-item)
  "docstring"
  (let ((label (car memo-item))
        (prompt
         (s-pad-right
            (memo--padding (length (cadr memo-item)))
            " "
            (cadr memo-item)))
        (fn (caddr memo-item)))
    (concat label (read-string prompt (funcall fn)))))


(defun memo--longest (lst)
  (apply #'max
         (mapcar #'length lst)))

(defun memo--colon (s)
  (concat s ":"))

(defun memo--padding  (n)
  (+ n 2))

(defun memo--item-jusify (s pad-size)
  (s-pad-right pad-size " " s))

(defun memo--paddify (lst)
  (mapcar
   (lambda (s) (memo--item-jusify s (memo--padding (memo--longest lst))))
   lst))

;;--------------------------------------------------
(defun memo--recipient ()
  "Returns a default recipient string."
  "<recipient>")

(defun memo--subject ()
  "Returns a default subject string."
  "<subject>")

;;--------------------------------------------------


(defun memo--read-string-nil (prompt &optional initial-input history default-value inherit-input-method)
  (let ((resp (read-string prompt initial-input history default-value inherit-input-method)))
    (when ((not equal) resp "")
      resp)))

(provide 'memo)

;;; memo.el ends here
