
#|===================================================
Lkit loader for UT/AI 1201/0502
=====================================================
|#


(defparameter lkit-root-path "C:/Users/Edward/Documents/Uni/AI applications/ICA3/AI-NLP-CD")

;; set environment to modern & warn user
(format t "Loading environment assumed MODERN, setting :MODERN")
(setf *features* (cons :MODERN *features*))

;; bootstrap Lkit
(let ((files '("x6-1(1c).lsp"        ; 3 Lkit files
               "x6-2(1c).lsp"
               "x6-3(1c).lsp"
               "rule-interface(1.3b).cl"
               "slot-parser(1.4).cl"
               )))
   (dolist (f files 'ok)
      (load (concatenate 'string lkit-root-path "\\" f))
      ))


