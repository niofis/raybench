(defparameter *width* 1280)
(defparameter *height* 720)
(defparameter *samples* 50)
(defparameter *max-depth* 5)

(defmacro v-x (v)
  `(nth 0 ,v))

(defmacro v-y (v)
  `(nth 1 ,v))

(defmacro v-z (v)
  `(nth 2 ,v))

(defun v-add (v1 v2)
  (mapcar #'+ v1 v2))

(defun v-sub (v1 v2)
  (mapcar #'- v1 v2))

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
  (v-div-s v1 (v-norm v1)))

;;Ray stuff
(defun ray-new (origin direction) 
  (list origin direction))

(defmacro ray-origin (ray) 
  `(nth 0 ,ray))

(defmacro ray-direction (ray) 
  `(nth 1 ,ray))

(defun ray-point (ray dist)
  (v-add (ray-origin ray) (v-mul-s (ray-direction ray) dist)))

;;Hit stuff
(defun hit-new (&optional dist point normal sphere) 
  (list dist point normal sphere))

(defmacro hit-distance (hit)
  `(nth 0 ,hit))

(defmacro hit-point (hit)
  `(nth 1 ,hit))

(defmacro hit-normal (hit)
  `(nth 2 ,hit))

(defmacro hit-sphere (hit)
  `(nth 3 ,hit))


;;Camera
(defmacro camera-new (eye lt rt lb)
  `(list ,eye ,lt ,rt ,lb))

(defmacro camera-eye (camera)
  `(nth 0 ,camera))

(defmacro camera-lt (camera)
  `(nth 1 ,camera))

(defmacro camera-rt (camera)
  `(nth 2 ,camera))

(defmacro camera-lb (camera)
  `(nth 3 ,camera))

;;Sphere
(defun sphere-new (center radius color is_light)
  (list center radius color is_light))

(defun sphere-center (sphere)
  (nth 0 sphere))

(defun sphere-radius (sphere)
  (nth 1 sphere))

(defun sphere-color (sphere)
  (nth 2 sphere))

(defun sphere-is-light (sphere)
  (nth 3 sphere))

(defun sphere-hit (sphere ray)
  (let* ((oc (v-sub (ray-origin ray) (sphere-center sphere)))
         (dir (ray-direction ray))
         (a (v-dot dir dir))
         (b (v-dot oc dir))
         (c (- (v-dot oc oc) (* (sphere-radius sphere) (sphere-radius sphere))))
         (dis (- (* b b) (* a c)))
         (e (sqrt dis))
         (t1 (/ (- (- 0 b) e) a))
         (t2 (/ (+ (- 0 b) e) a)))
    (cond ((and (> dis 0) (> t1 0.007))
           (let ((point (ray-point ray t1)))
             (hit-new t1 point (v-unit (v-sub point (sphere-center sphere))) sphere)))
          ((and (> dis 0) (> t2 0.007))
           (let ((point (ray-point ray t2)))
             (hit-new t2 point (v-unit (v-sub point (sphere-center sphere))) sphere)))
          (t (hit-new 1e15 nil nil nil)))))
         

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
  (nth 1 world))

(defun rnd-dome (normal)
  (let ((p '(0 0 0))
        (d 0))
    (loop
      (setf p (list (- (* 2 (random 1.0)) 1)
                (- (* 2 (random 1.0)) 1)
                (- (* 2 (random 1.0)) 1)))
      (setf p (v-unit p))
      (setf d (v-dot p normal))
      (when (> d 0) (return p)))))

(defun trace-ray (world ray depth)
  (let ((hits (loop for sp in (world-spheres world) collect (sphere-hit sp ray)))
        (hit (hit-new 1e16 nil nil nil))
        (color '(0 0 0)))
    (loop for lh in hits do
          (cond ((< (hit-distance lh) (hit-distance hit))
                  (setf hit lh))))
    (setf color (sphere-color (hit-sphere hit)))
    (cond ((null (hit-point hit)) '(0 0 0))
          ((>= depth *max-depth*) '(0 0 0))
          ((sphere-is-light (hit-sphere hit)) color)
          (t (let* ((nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit))))
                  (ncolor (trace-ray world nray (+ depth 1)))
                  (at (v-dot (ray-direction nray) (hit-normal hit))))
               (v-mul color (v-mul-s ncolor at)))))))
       

(defun to-255 (color)
  (let ((c255 (mapcar #'* color '(255.99 255.99 255.99))))
    (mapcar #'floor c255)))
                
(defun writeppm (data) 
  (with-open-file (ppm "lisprb.ppm" :direction :output :if-exists :supersede)
    (format ppm "P3~%~A ~A~%255~%" *width* *height*)
    (loop for row in data do
          (loop for color in row do
                (format ppm "~{~A ~}" (to-255 color)))
          (format ppm "~%"))))

(defun main ()
  (let* ((world (world-new))
        (camera (world-camera world))
        (lt (camera-lt camera))
        (vdu (v-div-s (v-sub (camera-rt camera) (camera-lt camera)) *width*))
        (vdv (v-div-s (v-sub (camera-lb camera) (camera-lt camera)) *height*))
        (data (loop for y from 0 to (- *height* 1) collect
                (loop for x from 0 to (- *width* 1) collect
                  (let* ((color '(0 0 0))
                         (ray (ray-new (camera-eye camera) '(0 0 0))))
                    (loop for i from 1 to *samples* do
                      (let ((dir '(0 0 0)))
                        (setf dir (v-add lt (v-add
                                            (v-mul-s vdu (+ x (random 1.0)))
                                            (v-mul-s vdv (+ y (random 1.0))))))
                        (setf dir (v-sub dir (ray-origin ray)))
                        (setf dir (v-unit dir))
                        (setf (ray-direction ray) dir)
                        (setf color (v-add color (trace-ray world ray 0)))))
                    (v-div-s color *samples*))))))
    (writeppm data)))
