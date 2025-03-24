;;; package --- Summary
;;; Commentary:
;;; Code:

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
  (let ((banner-file (expand-file-name "ascii/rei.txt" user-emacs-directory)))
    (if (file-exists-p banner-file)
	(let* ((banner (with-temp-buffer
			(insert-file-contents banner-file)
			(buffer-string))) ;; read banner file
	      (splash-text
	       (progn
		 (with-temp-buffer
		   (insert-text-button "www.gnu.org"
				       'action (lambda (_) (browse-url "https://www.gnu.org"))
				       'help-echo "visit www.gnu.org website"
				       'follow-link t)
		   (center-line) (insert "\n")
		   (insert (concat
		      (propertize "GNU Emacs" 'face 'bold)
		      " version "
		      (format "%d.%d" emacs-major-version emacs-minor-version)))
		   (center-line) (insert "\n")
	
		   (buffer-string))))
	  (combined-banner (concat banner "\n\n" splash-text))
	  (centered-banner (center-text-horizontally combined-banner)))
	      
	  (switch-to-buffer "*splash*")
	  (erase-buffer)
	  (insert centered-banner)) ;; center contents horizontally

	  
	  (setq buffer-read-only t)
	  (setq mode-line-format nil)
	  (setq default-directory "~")
	  ))
      (message "banner file not found!"))

(add-hook 'emacs-startup-hook 'show-startup-banner)

(setq inhibit-splash-screen t)

;; for testing
 (show-startup-banner)

(provide 'custom-splash)
;;; custom-splash.el ends here
