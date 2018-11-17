(require 'cl)
(require 'weather-api)
(require 'weather-display)



(defconst weather--buffer-name "* Weather *")

(defun weather (location)
  (interactive "MWhere are you? ")
  (pop-to-buffer weather--buffer-name)
  (erase-buffer)
  (let ((split-location (split-string location)))
    (weather-display (car split-location)
                     (apply 'weather-get split-location))))

(provide 'weather)
