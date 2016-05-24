(defparameter *width* 1280)
(defparameter *height* 720)
(defparameter *samples* 50)
(defparameter *max-depth* 5)

(defun v-add (v1 v2)
  (mapcar #'+ v1 v2))

(defun v-sub (v1 v2)
  (mapcar #'- (v1 v2)))

(defun v-mul (v1 v2)
  (mapcar #'* v1 v2))

(defun v-mul-s (v1 s)
    (v-mul v1 (list s s s)))

(defun v-div (v1 v2)
  (mapcar #'/ v1 v2))

(defun v-div-s (v1 s)
    (v-div v1 (list s s s)))

(defun v-dot (v1 v2)
  (apply #'+ (v-mul v1 v2)))

(defun v-norm (v1)
  (sqrt (v-dot v1 v1)))

(defun v-unit (v1)
  (v-div v1 (v-norm v1)))

;;Ray stuff
(defun ray-new (origin direction) 
  (list origin direction))

(defun ray-origin (ray) 
  (nth 0 ray))

(defun ray-direction (ray) 
  (nth 1 ray))

;;Hit stuff
(defun hit-new (dist point normal) 
  (list dist point normal))

;;Camera
(defun camera-new (eye lt rt lb)
  (list eye lt rt lb))

(defun camera-eye (camera)
  (nth 0 camera))

(defun camera-lt (camera)
  (nth 1 camera))

(defun camera-rt (camera)
  (nth 2 camera))

(defun camera-lb (camera)
  (nth 3 camera))

;;Sphere
(defun sphere-new (center radius color is_light)
  (list center radius color is_light))

(defun sphere-hit (sphere ray)
  "To implement"
  nil)

(defun world-new ()
  (list (camera-new '(0 4.5 75) '(-8 9 50) '(8 9 50) '(-8 0 50))
        (list (sphere-new '(0 -10002 0) 9999 '(1 1 1) nil)
              (sphere-new '(-10012 0 0) 9999 '(1 0 0) nil)
              (sphere-new '(10012 0 0) 9999 '(0 1 0) nil)
              (sphere-new '(0 0 -10012) 9999 '(1 1 1) nil)
              (sphere-new '(0 10012 0) 9999 '(1 1 1) t)
              (sphere-new '(-5 0 2) 2 '(1 1 0) nil)
              (sphere-new '(0 5 -1) 4 '(1 0 0) nil)
              (sphere-new '(8 5 -1) 2 '(0 0 1) nil))))

(defun world-camera (world)
  (nth 0 world))

(defun world-spheres (world)
  (nth 2 world))

(defun writeppm (data) 
  (with-open-file (ppm "lisprb.ppm" :direction :output :if-exists :supersede)
    (format ppm "P3~%~A ~A~%255~%" *width* *height*)
    ))

(defun main ()
  (let* ((world (world-new))
        (camera (world-camera world))
        (vdu (v-div-s (v-sub (camera-rt camera) (camera-lt camera)) *width*))
        (vdv (v-div-s (v-sub (camera-lb camera) (camera-lt camera)) *height*)))
    (loop for y from 0 to *height* collect
          (loop for x from 0 to *width* collect
                '(0 0 1)))))


;;(main)
