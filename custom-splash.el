;;; package --- Summary
;;; Commentary:


(defun center-text-horizontally (text)
  "center text horizontally in the current window"
  (let* ((window-width (window-width)) ;; get the width of the current window
	 (text-lines (split-string text "\n")) ;; split the text into liens
	 (max-line-length (apply 'max (mapcar 'length text-lines)))) ;; get the longest line's length
    (mapconcat
     (lambda (line)
       (let ((spaces (max 0 (/ (- window-width max-line-length) 2)))) ;; calculate spaces before each line
	 (concat (make-string spaces ?\s) line))) ;; add spaces before the line
     text-lines "\n"))) ;; rejoin lines with newlines

(defun show-startup-banner ()
  "display a custom banner on startup"
  (let ((banner-file "~/.emacs.d/ascii/nerv.txt"))
    (if (file-exists-p banner-file)
	(let ((banner (with-temp-buffer
			(insert-file-contents banner-file)
			(buffer-string)))) ;; read banner file

	  (switch-to-buffer "*splash*")
	  (erase-buffer)
	  (insert (center-text-horizontally banner)) ;; center contents horizontally
	  
	  ;; center contents vertically
	  (goto-char (point-min))
	  
	  (let* ((window-height (window-height))
		 (banner-lines (count-lines (point-min) (point-max)))
		 (padding (/ (- window-height banner-lines) 2)))
	    ;; add padding before banner for vertical centering
	    (dotimes (_ padding)
	      (insert "\n")))

	  
	  (setq buffer-read-only t)
	  (setq mode-line-format nil)
	  (setq default-directory "~")
	  )
      (message "banner file not found!"))))

(add-hook 'emacs-startup-hook 'show-startup-banner)

(setq inhibit-splash-screen t)

(provide 'custom-splash)
;;; custom-splash.el ends here
