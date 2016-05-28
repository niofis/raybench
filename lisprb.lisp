(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))

(defparameter *width* 1280)
(defparameter *height* 720)
(defparameter *samples* 50)
(defparameter *max-depth* 5)

(defstruct (vec
             (:conc-name v-)
             (:constructor v-new (x y z))
             (:type (vector float)))
  x y z)

(defun v-add (v1 v2)
  (v-new (+ (v-x v1) (v-x v2)) (+ (v-y v1) (v-y v2)) (+ (v-z v1) (v-z v2))))

(defun v-sub (v1 v2)
  (v-new (- (v-x v1) (v-x v2)) (- (v-y v1) (v-y v2)) (- (v-z v1) (v-z v2))))

(defun v-mul (v1 v2)
  (v-new (* (v-x v1) (v-x v2)) (* (v-y v1) (v-y v2)) (* (v-z v1) (v-z v2))))

(defun v-mul-s (v1 s)
  (v-new (* (v-x v1) s) (* (v-y v1) s) (* (v-z v1) s)))

(defun v-div (v1 v2)
  (v-new (/ (v-x v1) (v-x v2)) (/ (v-y v1) (v-y v2)) (/ (v-z v1) (v-z v2))))

(defun v-div-s (v1 s)
  (v-new (/ (v-x v1) s) (/ (v-y v1) s) (/ (v-z v1) s)))

(defun v-dot (v1 v2)
  (+ (* (v-x v1) (v-x v2)) (* (v-y v1) (v-y v2)) (* (v-z v1) (v-z v2))))
  
(defun v-norm (v1)
  (sqrt (v-dot v1 v1)))

(defun v-unit (v1)
  (v-div-s v1 (v-norm v1)))

(defstruct (ray
             (:conc-name ray-)
             (:constructor ray-new (origin direction))
             (:type vector))
  origin direction)

(defun ray-point (ray dist)
  (v-add (ray-origin ray) (v-mul-s (ray-direction ray) dist)))

(defstruct (hit
             (:conc-name hit-)
             (:constructor hit-new (distance point normal sphere))
             (:type vector))
  distance point normal sphere)

(defstruct (camera 
             (:conc-name camera-)
             (:constructor camera-new (eye lt rt lb))
             (:type vector))
  eye lt rt lb)

(defstruct (sphere
             (:conc-name sphere-)
             (:constructor sphere-new (center radius color is-light))
             (:type vector))
  center radius color is-light)

(defun sphere-hit (sphere ray)
  (let* ((oc (v-sub (ray-origin ray) (sphere-center sphere)))
         (dir (ray-direction ray))
         (a (v-dot dir dir))
         (b (v-dot oc dir))
         (c (- (v-dot oc oc) (* (sphere-radius sphere) (sphere-radius sphere))))
         (dis (- (* b b) (* a c))))
    (if (> dis 0)
      (let* ((e (sqrt dis))
             (t1 (/ (- (- b) e) a))
             (t2 (/ (+ (- b) e) a)))
        (if (> t1 0.007)
          (let ((point (ray-point ray t1)))
            (hit-new t1 point (v-unit (v-sub point (sphere-center sphere))) sphere))
          (if (> t2 0.007)
            (let ((point (ray-point ray t2)))
              (hit-new t1 point (v-unit (v-sub point (sphere-center sphere))) sphere))))))))
         

(defun world-new ()
  (list (camera-new #(0.0 4.5 75.0) #(-8.0 9.0 50.0) #(8.0 9.0 50.0) #(-8.0 0.0 50.0))
        (list (sphere-new #(0.0 -10002.0 0.0) 9999.0 #(1.0 1.0 1.0) nil)
              (sphere-new #(-10012.0 0.0 0.0) 9999.0 #(1.0 0.0 0.0) nil)
              (sphere-new #(10012.0 0.0 0.0) 9999.0 #(0.0 1.0 0.0) nil)
              (sphere-new #(0.0 0.0 -10012.0) 9999.0 #(1.0 1.0 1.0) nil)
              (sphere-new #(0.0 10012.0 0.0) 9999.0 #(1.0 1.0 1.0) t)
              (sphere-new #(-5.0 0.0 2.0) 2.0 #(1.0 1.0 0.0) nil)
              (sphere-new #(0.0 5.0 -1.0) 4.0 #(1.0 0.0 0.0) nil)
              (sphere-new #(8.0 5.0 -1.0) 2.0 #(0.0 0.0 1.0) nil))))

(defun world-camera (world)
  (nth 0 world))

(defun world-spheres (world)
  (nth 1 world))

(defun rnd-dome (normal)
  (let ((p #(0.0 0.0 0.0))
        (d 0))
    (loop
      (setf p (v-new (- (* 2.0 (random 1.0)) 1.0)
                (- (* 2.0 (random 1.0)) 1.0)
                (- (* 2.0 (random 1.0)) 1.0)))
      (setf p (v-unit p))
      (setf d (v-dot p normal))
      (when (> d 0) (return p)))))

(defun trace-ray (world ray depth)
  (let ((hits (loop for sp in (world-spheres world) collect (sphere-hit sp ray)))
        (hit (hit-new 1e16 nil nil nil))
        (color #(0.0 0.0 0.0)))
    (loop for lh in hits do
          (if (and (not (null lh)) (< (hit-distance lh) (hit-distance hit))) (setf hit lh)))
    (setf color (sphere-color (hit-sphere hit)))
    (cond ((null (hit-point hit)) #(0.0 0.0 0.0))
          ((>= depth *max-depth*) #(0.0 0.0 0.0))
          ((sphere-is-light (hit-sphere hit)) color)
          (t (let* ((nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit))))
                  (ncolor (trace-ray world nray (+ depth 1)))
                  (at (v-dot (ray-direction nray) (hit-normal hit))))
               (v-mul color (v-mul-s ncolor at)))))))
       
(defun to-255 (color)
  (map 'vector #'floor (v-mul-s color 255.99)))
                
(defun writeppm (data) 
  (with-open-file (ppm "lisprb.ppm" :direction :output :if-exists :supersede)
    (format ppm "P3~%~A ~A~%255~%" *width* *height*)
    (loop for row in data do
          (loop for color in row do
                (format ppm "~{~A ~}" (coerce (to-255 color) 'list)))
          (format ppm "~%"))))

(defun main (argv)
  (let* ((world (world-new))
        (camera (world-camera world))
        (lt (camera-lt camera))
        (vdu (v-div-s (v-sub (camera-rt camera) (camera-lt camera)) *width*))
        (vdv (v-div-s (v-sub (camera-lb camera) (camera-lt camera)) *height*))
        (data (loop for y from 0.0 to (- *height* 1.0) collect
                (loop for x from 0.0 to (- *width* 1.0) collect
                  (let ((color #(0.0 0.0 0.0))
                         (ray (ray-new (camera-eye camera) nil))
                         (dir nil))
                    (loop for i from 1 to *samples* do
                        (setf dir (v-add lt (v-add
                                            (v-mul-s vdu (+ x (random 1.0)))
                                            (v-mul-s vdv (+ y (random 1.0))))))
                        (setf dir (v-unit (v-sub dir (ray-origin ray))))
                        (setf (ray-direction ray) dir)
                        (setf color (v-add color (trace-ray world ray 0))))
                    (v-div-s color *samples*))))))
    (writeppm data)))

(main nil)
