
(defun weather--get-temp (weather-data)
  (alist-get 'temp
             (alist-get 'main weather-data)))

(defun weather-display (city-name weather-data)
  (insert city-name " weather:\n")
  (insert (format "%d degrees Fahrenheit\n" (weather--get-temp weather-data))))

(provide 'weather-display)
