;; Rurema for emacs
;; Please refer to "http://github.com/yhara/myrurema" for Rurema. 
;; 
;; This file is based on 'refe.el'
;;
; To install, put this in your .emacs:
;
; (require 'rurema)
; 

(defvar rurema-program-name
  (if (featurep 'meadow)
      "ruby"
    "rurema"))
(defvar rurema-program-args
  (if (featurep 'meadow)
      '("-S" "rurema")
    nil))
(defvar rurema-buffer-name "*Rurema*")
(defvar rurema-completion-table nil)

(defun rurema-call-process (buf &rest args)
  (let ((coding-system-for-read 'euc-japan))
    (apply 'call-process rurema-program-name nil buf nil
           (append rurema-program-args args))))

(defun rurema-make-completion-table ()
  (setq rurema-completion-table (make-vector 547 0))
  (with-temp-buffer
    (rurema-call-process t)
    (goto-char (point-min))
    (while (looking-at "[A-Z][A-Za-z_0-9:]*")
      (intern (match-string 0) rurema-completion-table)
      (skip-chars-forward "^ \n")
      (skip-chars-forward " \n")))

  (with-temp-buffer
    (rurema-call-process t "")
    (goto-char (point-min))
    (while (looking-at "\\(\\$[^ \n]*\\|[A-Z][A-Za-z_0-9:]*[#.][^ \n]*\\)")
      (intern (match-string 0) rurema-completion-table)
      (skip-chars-forward "^ \n")
      (skip-chars-forward " \n")))
  nil)

(rurema-make-completion-table)

(defun rurema-get-word-at-point ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun rurema (&optional word)
  (interactive)
  (if (null rurema-completion-table)
      (rurema-make-completion-table))
  (let* ((default (rurema-get-word-at-point))
	 (completion-ignore-case t)
	 (buf (or (get-buffer rurema-buffer-name)
		  (generate-new-buffer rurema-buffer-name)))
	 (pop-up-windows t)
	 (pop-up-frames nil))

    (if (null word)
	(setq word (completing-read
		    (if default
			(format "Class or Method (default %s): " default)
		      "Class or Method: ")
		    rurema-completion-table nil nil nil nil default)))

    (set-buffer buf)
    (setq buffer-read-only nil)
    (erase-buffer)

    (rurema-call-process buf word)

    (goto-char (point-min))
    (if (re-search-forward "^---" nil t)
	nil
      (let ((klass-table (make-vector 17 0))
	    (completion-ignore-case t)
	    klass)
	(goto-char (point-min))
	(while (looking-at "\\([A-Z][A-Za-z_0-9:]*\\)[#.][^ \n]*")
	  (intern (match-string 1) klass-table)
	  (skip-chars-forward "^ \n")
	  (skip-chars-forward " \n"))
	(if (= (point) (point-min))
	    nil ; (message "no such class or method: %s" word)
	  (setq klass (completing-read "Class: "
				       klass-table nil nil nil nil nil))
	  (erase-buffer)
	  (rurema-call-process buf "--all" klass word))))

    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (display-buffer buf)))

(provide 'rurema)
