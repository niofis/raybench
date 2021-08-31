(defpackage :lisprb
  (:use :cl))
(in-package :lisprb)

(declaim (optimize (speed 3) (safety 0)))

(defconstant +width+ 1280)
(defconstant +height+ 720)
(defconstant +samples+ 50)
(defconstant +max-depth+ 5)
(defconstant +float-type+ 'single-float)
(setf *read-default-float-format* +float-type+)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype float-type (&optional low high) `(,+float-type+ ,low ,high))
  (deftype vec-type () '(simple-array float-type (3)))

  (declaim (inline zero))
  (defun zero ()
    (ecase +float-type+
      (single-float 0.0s0)
      (double-float 0.0d0)))

  (declaim (inline %v v-x v-y v-z))
  (defstruct (vec (:conc-name v-)
                  (:constructor %v (x y z))
                  (:type (vector float-type)))
    (x (zero) :type float-type)
    (y (zero) :type float-type)
    (z (zero) :type float-type))

  (declaim (inline v))
  (defun v (x y z &optional output)
    (declare (type (or vec-type null)))
    (if output
        (progn
          (setf (v-x output) (coerce x +float-type+)
                (v-y output) (coerce y +float-type+)
                (v-z output) (coerce z +float-type+))
          output)
        (%v (coerce x +float-type+)
            (coerce y +float-type+)
            (coerce z +float-type+)))))

(defmacro define-v-op (name (a b) op &optional scalar)
  (let ((destructive-name (intern (format nil "~A!" name))))
    `(progn
       (declaim (inline ,name ,destructive-name))
       (defun ,name (,a ,b)
         (declare (type vec-type ,a)
                  (type ,(if scalar 'float-type 'vec-type) ,b))
         (v ,@(loop for acc in '(v-x v-y v-z)
                    collect `(,op (,acc ,a)
                                  ,(if scalar b `(,acc ,b ))))))
       (defun ,destructive-name (,a ,b)
         "destructively modify the first argument, then return it."
         (declare (type vec-type ,a)
                  (type ,(if scalar 'float-type 'vec-type) ,b))
         ,@(loop for acc in '(v-x v-y v-z)
                 collect `(setf (,acc ,a)
                                (,op (,acc ,a)
                                     ,(if scalar b `(,acc ,b )))))
         ,a))))

(define-v-op v-add (v1 v2) +)
(define-v-op v-sub (v1 v2) -)
(define-v-op v-mul (v1 v2) *)
(define-v-op v-mul-s (v1 s) * t)
(define-v-op v-div (v1 v2) /)
(define-v-op v-div-s (v1 s) / t)

(declaim (inline v-dot v-norm v-unit v-unit!))
(defun v-dot (v1 v2)
  (declare (type vec-type v1 v2))
  (+ (* (v-x v1) (v-x v2))
     (* (v-y v1) (v-y v2))
     (* (v-z v1) (v-z v2))))

(eval-when (:compile-toplevel)
  (sb-c:defknown (%sqrt)
      ((single-float 0.0s0)) (single-float 0.0s0)
      (sb-c:movable sb-c:foldable sb-c:flushable))

  (sb-c:define-vop (fsqrt/s)
    (:args (x :scs (sb-vm::single-reg)))
    (:results (y :scs (sb-vm::single-reg)))
    (:translate %sqrt)
    (:policy :fast-safe)
    (:arg-types single-float)
    (:result-types single-float)
    (:note "inline float arithmetic")
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator 1
                (unless (SB-C:location= x y)
                  (SB-C::inst SB-X86-64-ASM::xorpd y y))
                (SB-VM::note-float-location 'sqrt vop x)
                (SB-C::inst SB-X86-64-ASM::sqrtss y x))))

(defun %sqrt (x) (%sqrt x))

(defun v-norm (v1)
  (declare (type vec-type v1))
  (let ((d (v-dot v1 v1)))
    (declare (type (single-float 0.0)))
    (%sqrt d)))

(disassemble #'%sqrt)
(disassemble #'v-norm)

(defun v-unit (v1)
  (declare (type vec-type v1))
  (v-div-s v1 (v-norm v1)))

(defun v-unit! (v1)
  (declare (type vec-type v1))
  (v-div-s! v1 (v-norm v1)))

(declaim (inline ray-new))
(defstruct (ray
            (:constructor ray-new (origin direction)))
  (origin #.(v 0 0 0) :type vec-type)
  (direction #.(v 0 0 0) :type vec-type))

(declaim (inline ray-point))
(defun ray-point (ray dist &optional (v (v 0 0 0)))
  (declare (type ray)
           (type float-type dist)
           (type vec-type v))
  (replace v (ray-direction ray))
  (v-add! (v-mul-s! v dist)
          (ray-origin ray)))

(declaim (inline sphere-new))
(declaim (inline sphere-radius))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (sphere
              (:constructor sphere-new (center radius color is-light)))
    (center #.(v 0 0 0) :type vec-type)
    (radius 0.0 :type float-type)
    (color #.(v 0 0 0) :type vec-type)
    (is-light nil :type (or t nil))))


(declaim (inline hit-new))
(declaim (inline hit-distance))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (hit
              (:constructor hit-new (distance point normal sphere)))
    (distance 0.0 :type float-type)
    (point #.(v 0 0 0) :type vec-type)
    (normal #.(v 0 0 0) :type vec-type)
    (sphere (make-sphere) :type sphere)))

(declaim (inline camera-new))
(defstruct (camera
            (:constructor camera-new (eye lt rt lb)))
  (eye #.(v 0 0 0) :type vec-type)
  (lt #.(v -1.0 1.0 1.0) :type vec-type)
  (rt #.(v 1.0 1.0 1.0) :type vec-type)
  (lb #.(v -1.0 0.0 1.0) :type vec-type))


(defconstant +no-hit+ (if (boundp '+no-hit+)
                          +no-hit+
                          (hit-new 1e16 #.(v 0 0 0) #.(v 0 0 0)
                                   (sphere-new #.(v 0 0 0) 0.0 #.(v 0 0 0) nil))))

(defun sphere-hit (sphere ray output)
  (declare (type sphere sphere)
           (type ray ray)
           (type hit output))
  (let* ((oc (v-sub (ray-origin ray) (sphere-center sphere)))
         (dir (ray-direction ray))
         (a (v-dot dir dir))
         (b (v-dot oc dir))
         (c (- (v-dot oc oc)
               (* (sphere-radius sphere) (sphere-radius sphere))))
         (dis (- (* b b) (* a c))))
    (declare (dynamic-extent oc))
    (if (> dis 0.0)
        (let* ((e (sqrt dis))
               (t1 (/ (- (- b) e) a)))
          (if (> t1 0.007)
              (let ((point (ray-point ray t1 (hit-point output)))
                    (normal (hit-normal output)))
                (replace normal point)
                (v-unit! (v-sub! normal (sphere-center sphere)))
                (setf (hit-distance output) t1
                      (hit-sphere output) sphere)
                output)
              (let ((t2 (/ (+ (- b) e) a)))
                (if (> t2 0.007)
                    (let ((point (ray-point ray t2 (hit-point output)))
                          (normal (hit-normal output)))
                      (replace normal point)
                      (v-unit! (v-sub! normal (sphere-center sphere)))
                      (setf (hit-distance output) t2
                            (hit-sphere output) sphere)
                      output)
                    nil))))
        nil)))


(defun world-new ()
  (list (camera-new #.(v 0.0 4.5 75.0)
                    #.(v -8.0 9.0 50.0)
                    #.(v 8.0 9.0 50.0)
                    #.(v -8.0 0.0 50.0))
        (list (sphere-new #.(v 0.0 -10002.0 0.0) 9999.0 #.(v 1.0 1.0 1.0) nil)
              (sphere-new #.(v -10012.0 0.0 0.0) 9999.0 #.(v 1.0 0.0 0.0) nil)
              (sphere-new #.(v 10012.0 0.0 0.0) 9999.0 #.(v 0.0 1.0 0.0) nil)
              (sphere-new #.(v 0.0 0.0 -10012.0) 9999.0 #.(v 1.0 1.0 1.0) nil)
              (sphere-new #.(v 0.0 10012.0 0.0) 9999.0 #.(v 1.0 1.0 1.0) t)
              (sphere-new #.(v -5.0 0.0 2.0) 2.0 #.(v 1.0 1.0 0.0) nil)
              (sphere-new #.(v 0.0 5.0 -1.0) 4.0 #.(v 1.0 0.0 0.0) nil)
              (sphere-new #.(v 8.0 5.0 -1.0) 2.0 #.(v 0.0 0.0 1.0) nil))))

(defun world-camera (world)
  (first world))

(defun world-spheres (world)
  (second world))

;;https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
(declaim (ftype (function () (values (unsigned-byte 32) &optional)) xor128))
(let ((x 123456789)
      (y 362436069)
      (z 521288629)
      (w 88675123))
  (declare (type (unsigned-byte 32) x y z w))
  (defun xor128 ()
    (let ((temp (ldb (byte 32 0) (logxor x (ash x 11)))))
      (declare (type (unsigned-byte 32) temp))
      (shiftf x y z w
              (logxor w
                      (ldb (byte 13 19) w)
                      (logxor temp
                              (ldb (byte 24 8) temp))))
      w)))
(declaim (inline randf))
(defun randf ()
  (/ (float (xor128) 1.0) #.(float #xffffffff 1.0)))

(declaim (inline rnd-dome))
(defun rnd-dome (normal &optional (v (v 0 0 0)))
  (declare (type vec-type normal))
  (loop for p = (v-unit! (v (- (* 2.0 (randf)) 1.0)
                            (- (* 2.0 (randf)) 1.0)
                            (- (* 2.0 (randf)) 1.0)
                            v))
        unless (<= (v-dot p normal) 0.0)
          return p))


(defun trace-ray (world ray depth)
  (declare (type ray ray) (type fixnum depth))
  (let* ((v1 (%v 0.0 0.0 0.0))
         (v2 (%v 0.0 0.0 0.0))
         (v3 (%v 0.0 0.0 0.0))
         (v4 (%v 0.0 0.0 0.0))
         (tmp (hit-new 1e16 v1 v2
                       (sphere-new #.(v 0 0 0) 0.0 #.(v 0 0 0) nil)))
         (hit (hit-new 1e16 v3 v4
                       (sphere-new #.(v 0 0 0) 0.0 #.(v 0 0 0) nil)))
         (nohit t))
    (declare (dynamic-extent tmp hit v1 v2 v3 v4))
    (loop for sp in (world-spheres world)
          for res = (sphere-hit sp ray tmp)
          when (and res
                    (> (hit-distance res) 0.0001)
                    (< (hit-distance res)
                       (hit-distance hit)))
            do (progn
                 (setf nohit nil
                       (hit-distance hit) (hit-distance res)
                       (hit-sphere hit)   (hit-sphere res))
                 (replace (hit-point hit) (hit-point res))
                 (replace (hit-normal hit) (hit-normal res))))
    (cond
      ;; base case : ensure new vector is returned
      ((or nohit
           (>= depth +max-depth+))
       (v 0 0 0))
      ;; base case : ensure new vector is returned
      ((sphere-is-light (hit-sphere hit))
       (copy-vec (sphere-color (hit-sphere hit))))
      (t
       (let* ((r (v 0 0 0))
              (nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit) r)))
              (ncolor (trace-ray world nray (1+ depth)))
              (at (v-dot (ray-direction nray) (hit-normal hit))))
         (declare (dynamic-extent r nray))
         (v-mul! (v-mul-s! ncolor at)
                 (sphere-color (hit-sphere hit))))))))

(declaim (inline to-255))
(defun to-255 (color)
  (map 'list #'floor (v-mul-s color 255.99)))


(defun writeppm (data)
  (with-open-file (ppm "lisprb.ppm" :direction :output :if-exists :supersede)
    (format ppm "P3~%~A ~A~%255~%" +width+ +height+)
    (loop for row in data
          do (loop for color in row
                   do (format ppm "~{~A ~}" (to-255 color)))
             (format ppm "~%"))))

(defun produce-data ()
  (let* ((world (world-new))
         (camera (world-camera world))
         (lt (camera-lt camera))
         (vdu (v-div-s! (v-sub (camera-rt camera) (camera-lt camera))
                        (coerce +width+ +float-type+)))
         (vdv (v-div-s! (v-sub (camera-lb camera) (camera-lt camera))
                        (coerce +height+ +float-type+)))
         (data (loop
                 for y fixnum from 0 below +height+
                 collect
                 (loop
                   for x fixnum from 0 below +width+
                   collect
                   (let ((color (v 0 0 0))
                         (ray (ray-new (camera-eye camera) #.(v 0 0 0)))
                         (dir nil))
                     (loop
                       repeat +samples+
                       do (setf dir (v-add!
                                     (v-add! (v-mul-s vdu (+ (coerce x +float-type+)
                                                             (randf)))
                                             lt)
                                     (v-mul-s vdv (+ (coerce y +float-type+)
                                                     (randf)))))
                          (setf (ray-direction ray)
                                (v-unit! (v-sub! dir (ray-origin ray))))
                          (setf color (v-add! color
                                              (trace-ray world ray 0))))
                     (v-div-s! color (coerce +samples+ +float-type+)))))))
    data))

(defun main ()
  (let ((data (time (produce-data))))
    (time (writeppm data))))


(defun dump ()
  (sb-ext:save-lisp-and-die "lisprb" :toplevel #'main :executable t))

