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
  (let ((banner  "
                                __ _._.,._.__
                          .o8888888888888888P'
                        .d88888888888888888K
          ,8            888888888888888888888boo._
         :88b           888888888888888888888888888b.
          `Y8b          88888888888888888888888888888b.
            `Yb.       d8888888888888888888888888888888b
              `Yb.___.88888888888888888888888888888888888b
                `Y888888888888888888888888888888CG88888P\"\'
                  `88888888888888888888888888888MM88P\"\'
 \"Y888K    \"Y8P\"\"Y888888888888888888888888oo._\"\"\"\"
   88888b    8    8888`Y88888888888888888888888oo.
   8\"Y8888b  8    8888  ,8888888888888888888888888o,
   8  \"Y8888b8    8888\"\"Y8`Y8888888888888888888888b.
   8    \"Y8888    8888   Y  `Y8888888888888888888888
   8      \"Y88    8888     .d `Y88888888888888888888b
 .d8b.      \"8  .d8888b..d88P   `Y88888888888888888888
                                  `Y88888888888888888b.
                   \"Y888P\"\"Y8b. \"Y888888888888888888888
                     888    888   Y888`Y888888888888888
                     888   d88P    Y88b `Y8888888888888
                     888\"Y88K\"      Y88b dPY8888888888P
                     888  Y88b       Y88dP  `Y88888888b
                     888   Y88b       Y8P     `Y8888888
                   .d888b.  Y88b.      Y        `Y88888
                                                  `Y88K
                                                    `Y8
                                                      '
"))
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
    ))

(add-hook 'emacs-startup-hook 'show-startup-banner)

(setq inhibit-splash-screen t)

(provide 'custom-splash)
;;; custom-splash.el ends here
