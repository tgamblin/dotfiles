;; c-header-switch.el
;; This file sets up a keyboard shortcut so that you can switch between source
;; and header files in C and C++ mode.
(defun reduce-helper (op list-of-values initial-value)
  (let ((first (car list-of-values))
        (rest  (cdr list-of-values)))
    (cond (rest (reduce-helper op rest (apply op (list initial-value first))))
          (first (apply op (list first initial-value)))
          (t initial-value))))

(defun reduce (op list) (reduce-helper op (cdr list) (car list)))

(defun squish (x y) (if x x y))

(defun map-squish (predicate list)
  (reduce 'squish (mapcar predicate list)))

(defun switch-to-buf-or-file (bufroot patterns)
  (let* ((names (mapcar (lambda (x) (format x bufroot)) patterns))
         (isbuf (map-squish 'get-buffer names)))
    (if isbuf (switch-to-buffer isbuf)
      (let ((isfile (map-squish (lambda (f) (if (file-exists-p f) f)) names)))
        (if isfile (find-file isfile) (c++-mode))))))

(defun cartesian (list-of-sets)
  (let ((first (car list-of-sets))
        (rest  (cdr list-of-sets)))
    (cond ((null rest) (mapcar 'list first))
          (t (reduce 'append
                     (mapcar (lambda (x)
                               (mapcar (lambda (y)
                                         (append y (list x)))
                                       (cartesian rest)))
                             first))))))

(defun string-cartesian (list-of-sets)
  (mapcar (lambda (x) (reduce 'concat x)) (cartesian list-of-sets)))

(defun my-goto-h-file ()
  (interactive)
  (let ((bufroot (file-name-sans-extension (buffer-name))))
    (cond ((or (string-match "\\.\\(cpp\\|C\\|c\\|cxx\\)$" (buffer-name)))
           (switch-to-buf-or-file
            bufroot (string-cartesian '(("" "../inc/" "../include/" "../h/")
                                        ("%s.h" "%s.H" "%s.hpp" "%s.hxx"))   )))

          ((string-match "\\.\\(h\\|H\\|hxx\\|hpp\\)$" (buffer-name))
           (switch-to-buf-or-file
            bufroot (string-cartesian '(("" "../src/") ("%s.cpp" "%s.cxx" "%s.C" "%s.c"))  )))

          ((or (string-match "\\.\\(ll\\|l\\)$" (buffer-name)))
           (switch-to-buf-or-file bufroot '("%s.yy" "%s.y")))

          ((or (string-match "\\.\\(yy\\|\\y\\)$" (buffer-name)))
           (switch-to-buf-or-file bufroot ("%s.ll" "%s.l"))))))

(eval-after-load "cc-mode" '(define-key c++-mode-map (kbd "C-j") 'my-goto-h-file))
(eval-after-load "cc-mode" '(define-key c-mode-map (kbd "C-j") 'my-goto-h-file))

; Tell emacs that we provide this feature
(provide 'c-header-switch)
