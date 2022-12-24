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
                   (safety 0)
                   (debug 0)
                   (compilation-speed 0)))

(defun big-sum-macro (n)
  (declare (type fixnum n))
  (loop :for i fixnum :below n
        :for val  = (fm:vec4 0.01 0.02 0.03 0.04)
          :then (fm:mv+ val val)
        :finally (return val)))

(defun big-sum-function (n)
  (declare (type fixnum n))
  (loop :for i fixnum :below n
        :for val = (fm:vec4 0.01 0.02 0.03 0.04)
          :then (fm:v+ val val)
        :finally (return val)))

(defun benchmark-big-sum (n)
  (format t "Calling (big-sum-macro 1000) ~d times.~%" n)
  (time
   (dotimes (i n)
     (declare (type fixnum i))
     (big-sum-macro 1000)))
  (format t "Calling (big-sum-function 1000) ~d times.~%" n)
  (time
   (dotimes (i n)
     (declare (type fixnum i))
     (big-sum-function 1000))))

(defun dot-product-macro (n)
  (dotimes (i n)
    (declare (type fixnum i))
    (mv. (mvec4-random -1.0 1.0)
         (mvec4-random -1.0 1.0))))

(defun dot-product-function (n)
  (dotimes (i n)
    (declare (type fixnum i))
    (v. (vec4-random -1.0 1.0)
          (vec4-random -1.0 1.0))))

(defun benchmark-dot-product (n)
  (format t "Calling (mv. 1000) ~d times.~%" n)
  (time (dot-product-macro n)
   )
  (format t "Calling (v. ) ~d times.~%" n)
  (time (dot-product-function n)
   ))
