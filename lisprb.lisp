(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))

(defconstant WIDTH 1280)
(defconstant HEIGHT 720)
(defconstant SAMPLES 2)
(defconstant MAX-DEPTH 5)


(defstruct (vec
             (:conc-name v-)
             (:constructor v-new (x y z))
             (:type (vector float)))
  x y z)

(defconstant ZERO (v-new 0.0 0.0 0.0))

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

(defconstant NOHIT (hit-new 1e16 ZERO ZERO (sphere-new ZERO 0.0 ZERO nil)))

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
              (hit-new t1 point (v-unit (v-sub point (sphere-center sphere))) sphere))
              NOHIT)))
      NOHIT)))
         

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
  (let ((p (v-new (- (* 2.0 (random 1.0)) 1.0)
                (- (* 2.0 (random 1.0)) 1.0)
                (- (* 2.0 (random 1.0)) 1.0))))
    (if (< (v-dot p normal) 0) (rnd-dome normal) p)))

(defun trace-ray (world ray depth)
  (let* ((hits (loop for sp in (world-spheres world) collect (sphere-hit sp ray)))
        (hit (reduce (lambda (h1 h2) (if (< (hit-distance h1) (hit-distance h2)) h1 h2)) hits))
        (color (sphere-color (hit-sphere hit))))
    (cond ((eq hit NOHIT) ZERO)
          ((sphere-is-light (hit-sphere hit)) color)
          ((< depth MAX-DEPTH) (let* ((nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit))))
                  (ncolor (trace-ray world nray (+ depth 1)))
                  (at (v-dot (ray-direction nray) (hit-normal hit))))
               (v-mul color (v-mul-s ncolor at))))
          (t ZERO))))
       
(defun to-255 (color)
  (map 'vector #'floor (v-mul-s color 255.99)))
                
(defun writeppm (data) 
  (with-open-file (ppm "lisprb.ppm" :direction :output :if-exists :supersede)
    (format ppm "P3~%~A ~A~%255~%" WIDTH HEIGHT)
    (loop for row in data do
          (loop for color in row do
                (format ppm "~{~A ~}" (coerce (to-255 color) 'list)))
          (format ppm "~%"))))

(defun main ()
  (let* ((world (world-new))
        (camera (world-camera world))
        (lt (camera-lt camera))
        (vdu (v-div-s (v-sub (camera-rt camera) (camera-lt camera)) WIDTH))
        (vdv (v-div-s (v-sub (camera-lb camera) (camera-lt camera)) HEIGHT))
        (data (loop for y from 0.0 to (- HEIGHT 1.0) collect
                (loop for x from 0.0 to (- WIDTH 1.0) collect
                  (let ((color ZERO)
                         (ray (ray-new (camera-eye camera) nil))
                         (dir nil))
                    (dotimes (_ SAMPLES)
                        (setf dir (v-add lt (v-add
                                            (v-mul-s vdu (+ x (random 1.0)))
                                            (v-mul-s vdv (+ y (random 1.0))))))
                        (setf dir (v-unit (v-sub dir (ray-origin ray))))
                        (setf (ray-direction ray) dir)
                        (setf color (v-add color (trace-ray world ray 0))))
                    (v-div-s color SAMPLES))))))
    (writeppm data)))

;(require :sb-sprof)
;(sb-sprof:start-profiling)    
(main)
;(sb-sprof:stop-profiling)
;(sb-sprof:report)

