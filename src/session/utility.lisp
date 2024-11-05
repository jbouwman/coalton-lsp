(in-package #:coalton-lsp)

;;; Coverting between character offsets and line/character position

(defun probe-offset (offsets index value)
  (let ((lo (nth index offsets))
        (hi (nth (1+ index) offsets)))
    (cond ((< value lo) -1)
          ((not hi)      0)
          ((< hi value)  1)
          (t             0))))

(defun find-line (offsets value)
  (loop :with low := 0
        :with high := (length offsets)
        :with index := (floor (/ (+ low high) 2))
        :do (case (probe-offset offsets index value)
              ( 0 (return (values index (nth index offsets))))
              (-1 (setf high index))
              ( 1 (setf low index)))
            (setf index (floor (/ (+ low high) 2)))))

(defun offsets->positions (offsets start end)
  (multiple-value-bind (start-line start-offset)
      (find-line offsets start)
    (multiple-value-bind (end-line end-offset)
        (find-line offsets end)
      `(:start
        (:line ,start-line
         :character ,(- start start-offset))
        :end
        (:line ,end-line
         :character ,(- end end-offset))))))
