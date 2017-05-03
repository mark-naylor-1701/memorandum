;; author: Mark W. Naylor
;; file:  memo.el
;; date:  2017-May-03

;; Package for automating creation of memoranda.

(setq lexical-binding t)


(setq memo-items
      '("TO" ; "CC"
        "FROM" "DATE" "SUBJ"))


(defun longest (lst)
  (apply #'max
         (mapcar #'length lst)
         ))

(defun colon (s)
  (concat s ":"))

(defun padding  (n)
  (+ n 2))

(defun item-jusify (s pad-size)
  (s-pad-right pad-size " " (colon s)))

(defun paddify (lst)
  (mapcar
   (lambda (s) (item-jusify s (padding (longest lst))))
   lst))


;; Replace temporary explicit handlers.
(defun from-handler (s)
  (concat s (read-string "from? " (user-full-name))))

(defun to-handler (s)
  (concat s (read-string "To? " "<recipient>")))

(defun subj-handler (s)
  (concat s (read-string "Subject? " "<subject>")))

(defun date-handler (s)
  (concat s (read-string "Date? " (short-date))))

;; (from-handler "FROM: ")
;; (to-handler   "TO:   ")
;; (subj-handler "SUBJ: ")
;; (date-handler "DATE: ")

(defun read-string-nil (prompt &optional initial-input history default-value inherit-input-method)
  (let ((resp (read-string prompt initial-input history default-value inherit-input-method)))
    (if (equal resp "")
        nil
      resp)))

(defun handler (prompt default prefix reader-fn)
  #'(lambda ()
    (concat prefix (funcall reader-fn prompt default)))
  )

;; (setq fh (handler "from? " (user-full-name) "FROM: " 'read-string))

;; (funcall fh)

(defun cc-list ()
  (let ((this (read-string-nil "Next recipent: ")))
    (if this
        (cons this (cc-list))
      '()))
  )


(cc-list)
