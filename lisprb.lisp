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

(defun v-div (v1 s)
    (v-div v1 (list s s s)))

(defun v-dot (v1 v2)
  (apply #'+ (v-mul v1 v2)))

(defun v-norm (v1)
  (sqrt (v-dot v1 v1)))

(defun v-unit (v1)
  (v-div v1 (v-norm v1)))

(defun ray-point ())


