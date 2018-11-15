
(defun weather--hierarchic-get (weather-data properties)
  (let ((properties (apply 'list properties))
        (current weather-data))
    (while properties
      (setq current (alist-get (car properties) current))
      (setq properties (rest properties)))
    current))

(defun weather--get-temp (weather-data)
  (weather--hierarchic-get weather-data '(main temp)))

(defun weather--get-sunrise (weather-data)
  (weather--hierarchic-get weather-data '(sys sunrise)))

(defun weather--get-sunset (weather-data)
  (weather--hierarchic-get weather-data '(sys sunset)))

(defun weather--all-sun-data (weather-data)
  (list (cons "sunrise" (weather--get-sunrise weather-data))
        (cons "sunset" (weather--get-sunset weather-data))))

(defun weather--interesting-sun-data (now weather-data)
  (seq-filter (lambda (x) (> (cdr x) now))
              (weather--all-sun-data weather-data)))

(defun weather--time-format-string (seconds)
  (let ((seconds (truncate seconds)))
   (apply 'concat (seq-filter (lambda (x) (not (null x)))
                              (list (when (>= seconds 3600)
                                      "%H ")
                                    (when (and (> seconds 60) (not (= 0 (mod seconds 3600))))
                                      "%M ")
                                    (when (not (= 0 (mod seconds 60)))
                                      "%S"))))))

(defun weather--format-time (seconds)
  (format-seconds (weather--time-format-string seconds) seconds))

(defun weather--present-sun-event (now sun-event)
  (insert (capitalize (car sun-event))
          " in "
          (weather--format-time (- (cdr sun-event) now))
          "\n"))

(defun weather--present-sun-data (now weather-data)
  (apply 'concat (seq-map (lambda (sun-event)
                            (weather--present-sun-event now sun-event))
                          (weather--interesting-sun-data now weather-data))))

(defun weather-display (city-name weather-data)
  (insert city-name " weather:\n")
  (insert (format "%d degrees Fahrenheit\n" (weather--get-temp weather-data)))
  (insert (weather--present-sun-data (float-time) weather-data)))

(provide 'weather-display)
