;; fast-math.lisp

;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :fast-math)

(declaim (optimize (speed 3)
                   (safety 3)
                   (debug 3)
                   (compilation-speed 0))
         (ftype (function ((array double-float) (array double-float)) double-float) simd-dot)
         (ftype (function ((array double-float) fixnum) f64.4) vec4-aref)
         (inline simd-dot
                 v+
                 v-
                 vec4
                 f64.4+
                 f64.4-
                 vec4-aref
                 vcross
                 v.))

(defun simd-dot (array1 array2 &aux (n (min (array-total-size array1) (array-total-size array2))))
  (declare (type (simple-array double-float 1) array1 array2)
           (optimize speed (safety 0)))
  (do
   ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
    (acc1 (f64.4 0)
          (f64.4+ acc1
                  (f64.4* (f64.4-row-major-aref array1 (+ index 0))
                          (f64.4-row-major-aref array2 (+ index 0)))))
    (acc2 (f64.4 0)
          (f64.4+ acc2
                  (f64.4* (f64.4-row-major-aref array1 (+ index 4))
                          (f64.4-row-major-aref array2 (+ index 4)))))
    (acc3 (f64.4 0)
          (f64.4+ acc3
                  (f64.4* (f64.4-row-major-aref array1 (+ index 8))
                               (f64.4-row-major-aref array2 (+ index 8)))))
    (acc4 (f64.4 0)
          (f64.4+ acc4
                  (f64.4* (f64.4-row-major-aref array1 (+ index 12))
                          (f64.4-row-major-aref array2 (+ index 12))))))
   ((>= index (- n 16))
    (do ((result (f64.4-horizontal+ (f64.4+ acc1 acc2 acc3 acc4))
                 (+ result (* (row-major-aref array1 index)
                              (row-major-aref array2 index))))
         (index index (1+ index)))
        ((>= index n) result)))))

(defun vec4-random (min max)
  (declare (optimize (speed 1))
           (type number min max))

  (vec4 (+ min (random (- max min)))
        (+ min (random (- max min)))
        (+ min (random (- max min)))
        (+ min (random (- max min)))))

(defmacro mvec4-random (min max)
  (declare (optimize (speed 1))
           (type number min max))
  (vec4 (+ min (random (- max min)))
        (+ min (random (- max min)))
        (+ min (random (- max min)))
        (+ min (random (- max min)))))

(defun vec4 (x y z w)
  (declare (type number x y z w))
  (sb-simd-avx2:make-f64.4 x y z w))

(defun components (vec)
  (declare (type sb-simd-avx2:f64.4 vec))
  (sb-simd-avx2:f64.4-values vec))

(defun vx (vec)
  (declare (type sb-simd-avx2:f64.4 vec))
  (multiple-value-bind (x y z w) (sb-simd-avx2:f64.4-values vec)
    (declare (ignorable y z w))
    x))

(defun vy (vec)
  (declare (type sb-simd-avx2:f64.4 vec))
  (multiple-value-bind (x y z w) (sb-simd-avx2:f64.4-values vec)
    (declare (ignorable x z w))
    y))

(defun vz (vec)
  (declare (type sb-simd-avx2:f64.4 vec))
  (multiple-value-bind (x y z w) (sb-simd-avx2:f64.4-values vec)
    (declare (ignorable x y w))
    z))

(defun vw (vec)
  (declare (type sb-simd-avx2:f64.4 vec))
  (multiple-value-bind (x y z w) (sb-simd-avx2:f64.4-values vec)
    (declare (ignorable x y z))
    w))

(defun vec3 (x y z)
  (declare (optimize (speed 1))
           (type number x y z))
  (sb-simd-avx2:make-f64.4 x y z 0.0d0))

(defun make-vec4-array (count)
  (make-array count :element-type 'double-float
                    :initial-element 0.0d0))

(defmacro mv+ (&rest args)
  `(f64.4+ ,@args))

(defmacro mv- (&rest args)
  `(f64.4- ,@args))

(defmacro mv. (a b)
  (let ((a-var (gensym "a-var"))
        (b-var (gensym "b-var")))
    `(let ((,a-var ,a)
          (,b-var ,b))
      (declare (type f64.4 ,a-var ,b-var))
      (sb-simd-avx2:f64.4-horizontal+ (f64.4* ,a-var ,b-var)))))

(defun v. (a b)
  (declare (type f64.4 a b))
  (sb-simd-avx2:f64.4-horizontal+ (f64.4* a b)))

(defun old-v. (a b)
  2oo`(mv. a b))


  ;; ( a2 * b3 - a3 * b2 ) * i
  ;; ( a1 * b3 - a3 * b1 ) * j
  ;; ( a1 * b2 - a2 * b1 ) * k
(defun vcross (a first second)
  (let ((perm1 (sb-simd-avx2:imm4 first))
        (perm2 (sb-simd-avx2:imm4 second)))
    (sb-simd-avx2:f64.4-permute128 a perm1 perm2)))

(defun v+ (&rest args)
     (apply #'f64.4+ args))

(defun v- (&rest args)
  (apply #'f64.4- args))

(defun vec4-aref (array idx)
  (declare (type f64vec array)
           (type fixnum idx))
  (f64.4-aref array idx))


(defun vsum (array &aux (n (array-total-size array)))
  (declare (type f64vec array)
           (optimize (speed 3) (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (f64.4 0) (f64.4+ acc1 (vec4-aref array (+ index 0))))
       (acc2 (f64.4 0) (f64.4+ acc2 (vec4-aref array (+ index 4))))
       (acc3 (f64.4 0) (f64.4+ acc3 (vec4-aref array (+ index 8))))
       (acc4 (f64.4 0) (f64.4+ acc4 (vec4-aref array (+ index 12)))))
      ((> index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (row-major-aref array index)))
            (index index (1+ index)))
           ((>= index n) result)))))
