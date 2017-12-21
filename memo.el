;;; memo.el --- Interactive creation of memoranda.

;; Copyright (©) 2017 Mark W. Naylor

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "25.0") (names "20171012.1214") (s "20171102.227"))
;; Keywords: foo bar baz
;; URL: http://example.com/jrhacker/superfrobnicate

(require 'names)
(require 's)

(setq lexical-binding t)

(define-namespace memo-

 (defun generate ()
   (interactive)
   (find-file (read-string "memo file name:  " "memo.txt"))

   (insert (s-join "\n" (append (mapcar #'-formatted-item (-items)) '(""))))
   (newline 2)
   (insert "MESSAGE BODY"))

 (defun -items-base ()
   (list
    (list "TO" (list "To" #'-recipient))
    (list "FROM" (list "From" #'user-full-name))
    (list "SUBJ" (list "Subject" #'-subject))
    (list "DATE" (list "Date" #'short-date))))

 (defun -items ()
   (let ((items (-items-base)))
     (-zip
      (-paddify (mapcar #'-colon (mapcar #'car items)))
      (mapcar #'cadr items))))

 (defun -formatted-item (item)
   "docstring"
   (let ((label (car item))
         (prompt
          (s-pad-right
           (-padding (length (cadr item)))
           " "
           (cadr item)))
         (fn (caddr item)))
     (concat label (read-string prompt (funcall fn)))))


 (defun -longest (lst)
   (apply #'max
          (mapcar #'length lst)))

 (defun -colon (s)
   (concat s ":"))

 (defun -padding  (n)
   (+ n 2))

 (defun -item-jusify (s pad-size)
   (s-pad-right pad-size " " s))

 (defun -paddify (lst)
   (mapcar
    (lambda (s) (-item-jusify s (-padding (-longest lst))))
    lst))

 ;;--------------------------------------------------
 (defun -recipient ()
   "Returns a default recipient string."
   "<recipient>")

 (defun -subject ()
   "Returns a default subject string."
   "<subject>")

 ;;--------------------------------------------------


 (defun -read-string-nil (prompt &optional initial-input history default-value inherit-input-method)
   (let ((resp (read-string prompt initial-input history default-value inherit-input-method)))
     (when ((not equal) resp "")
       resp)))

)                                       ;end of namespace


(provide 'memo)


;;; memo.el ends here
