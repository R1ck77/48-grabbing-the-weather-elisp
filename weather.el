(require 'cl)
(require 'json)

(defconst weather--api-file "apikey.txt")

(defun weather--read-key ()
  (with-temp-buffer
    (insert-file-contents weather--api-file)
    (buffer-substring (line-beginning-position)
                      (line-end-position))))

(defun weather--get-content-buffer (city-name country-code)
  (let ((url-request-method "POST"))
    (url-retrieve-synchronously
     (format "http://api.openweathermap.org/data/2.5/weather?q=%s,%s&appid=%s" city-name country-code (weather--read-key)))))

(defun weather--get (city-name country-code)
  (with-current-buffer (weather--get-content-buffer city-name country-code)
    (goto-char url-http-end-of-headers)
    (forward-line)
    (let ((result (buffer-substring (point) (point-max))))
      (kill-buffer)
      (json-read-from-string result))))
