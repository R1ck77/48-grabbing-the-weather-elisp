
(defun weather--hierarchic-get (weather-data properties)
  (let ((properties (apply 'list properties))
        (current weather-data))
    (while properties
      (setq current (alist-get (car properties) current))
      (setq properties (rest properties)))
    current))

(defun weather--get-temperature (weather-data)
  (weather--hierarchic-get weather-data '(main temp)))

(defun weather--get-humidity (weather-data)
  (weather--hierarchic-get weather-data '(main humidity)))

(defun weather--get-condition (weather-data)
  (alist-get 'description
             (aref (weather--hierarchic-get weather-data
                                            '(weather))
                   0)))

(defun weather--get-sunrise (weather-data)
  (weather--hierarchic-get weather-data '(sys sunrise)))

(defun weather--get-sunset (weather-data)
  (weather--hierarchic-get weather-data '(sys sunset)))

(defun weather--convert-speed (meters-per-seconds)
  (/ (* meters-per-seconds
        3600)
     1000))

(defun weather--get-wind-data (weather-data)
  "Returns (wind km/h . direction)"
  (let ((wind-alist (alist-get 'wind weather-data)))
    (cons (weather--convert-speed (alist-get 'speed wind-alist))
          (alist-get 'deg wind-alist))))

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

(defconst weather--directions '(( 22.5 . "N" )
                                ( 67.5 . "NE")
                                (112.5 . "E" )
                                (157.5 . "SE")
                                (202.5 . "S" )
                                (247.5 . "SW")
                                (292.5 . "W" )
                                (337.5 . "NW")
                                (382.5 . "N")))

(defun weather--present-direction (deg)
  (or (first
       (seq-filter (lambda (x)
                     (not (null x)))
                   (seq-map (lambda (x)
                              (if (< deg (car x)) (cdr x)))
                            weather--directions)))
      "N"))

(defun weather--present-wind-data (weather-data)
  (let ((wind-data (weather--get-wind-data weather-data)))
    (let ((speed (car wind-data))
          (deg (cdr wind-data)))
      (if (>= speed 0.5)
          (format "wind %skm/h %s\n" speed (weather--present-direction deg))
        "no wind\n"))))

(defun weather--kill-buffer ()
  (interactive)
  (kill-buffer))

(defun weather-display (city-name weather-data)
  (insert city-name "'s weather: ")
  (insert (format "%s\n%d℃  with %d%% of humidity\n"
                  (weather--get-condition weather-data)
                  (weather--get-temperature weather-data)
                  (weather--get-humidity weather-data)))
  (insert (weather--present-wind-data weather-data))
  (insert (weather--present-sun-data (float-time) weather-data))
  (setq buffer-read-only t)
  (goto-char (point-min))
  (local-set-key (kbd "q") 'weather--kill-buffer))

(provide 'weather-display)
