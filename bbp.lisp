(deftype constant ()
  '(member catalan log2 pi))

(defun power-mod (a n m &optional (r 1))
  "Calculate a^n mod m efficiently."
  (declare (type (integer 0) n)
           (type (integer 1) m)
           (type (integer 2) a))
  (if (= n 0) r
      (progn
        (setq a (mod (* a a) m)
              n (truncate n 2))
        (if (oddp n)
            (progn
              (setq r (mod (* r a) m))
              (power-mod a n m r))
            (power-mod a n m r)))))
