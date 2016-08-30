(defclass group ()
	((order :accessor group-order
					:initform 1
					:initarg :order)
	 (generator :accessor group-generator
							:initform 1
							:initarg :generator)))

(defclass group-element ()
	((value :accessor elem-value
					:initarg :value)
	 (group :accessor elem-group
					:initarg :group)))

(defgeneric g+ (a b))
(defgeneric g- (a b))
(defgeneric g* (a b))
(defgeneric g^ (base exponent))


(defmethod g+ ((a number) (b number))
	(+ a b))

(defmethod g- ((a number) (b number))
	(- a b))

(defmethod g* ((a number) (b number))
	(* a b))

(defmethod g+ ((a group-element) (b group-element))
	(make-instance 'group-element
								 :value (mod (+ (elem-value a) (elem-value b)) (group-order (elem-group a)))
								 :group (elem-group a)))

(defmethod g- ((a group-element) (b group-element))
	(make-instance 'group-element
								 :value (mod (- (elem-value a) (elem-value b)) (group-order (elem-group a)))
								 :group (elem-group a)))

(defmethod g* ((a group-element) (b group-element))
	(make-instance 'group-element
								 :value (mod (* (elem-value a) (elem-value b)) (group-order (elem-group a)))
								 :group (elem-group a)))

(defmethod g* ((a group-element) (b number))
	(make-instance 'group-element
								 :value (mod (* (elem-value a)  b) (group-order (elem-group a)))
								 :group (elem-group a)))

(defmethod g* ((a number) (b group-element))
	(make-instance 'group-element
								 :value (mod (* (elem-value b)  a) (group-order (elem-group b)))
								 :group (elem-group b)))

(defgeneric g-modinv (a b))

(defgeneric egcd (a b))

(defmethod egcd ((a number) (b number))
	(if (zerop a)
			(list b 0 1)
			(let* ((e (egcd (mod b a) a))
						 (g (first e))
						 (x (second e))
						 (y (third e)))
				(list g (- y (* (floor b a) x)) x))))

(defmethod g-modinv ((a number) (b number))
	(let* ((e (egcd a b))
				 (g (first e))
				 (x (second e)))
		(if (= g 1)
				(mod x b)
				(error "Can't get inverse modulo!"))))

(defun g^-1 (a)
	(make-instance 'group-element
								 :value (g-modinv (elem-value a)
																	(group-order (elem-group a)))
								 :group (elem-group a)))

(defmethod g^ ((base group-element) (exponent number))
	(if (> 0 exponent)
			(g^-1 (g^ base (* -1 exponent)))
			(if (= exponent 0)
					(make-instance 'group-element :value 1 :group (elem-group base))
					(if (= (mod exponent 2) 1)
							(g* base (g^ base (1- exponent)))
							(let ((d (g^ base (/ exponent 2))))
								(g* d d))))))

(defmethod g^ ((base group-element) (exponent group-element))
	(g^ base (elem-value exponent)))

(defun ggcd (a b)
 	(if (zerop b)
			a
			(ggcd b (mod a b))))

(defmethod print-object ((object group-element) stream)
	(format stream "(group-element ~d)" (elem-value object)))

(defun poly-eval (poly x)
	(if (= (length poly) 1)
			(first poly)
			(g+ (first poly) (g* (poly-eval (rest poly) x) x))))

(defun poly-mult (a b)
	(let ((result (make-list (1- (+ (length a) (length b))) :initial-element 0)))
		(dotimes (i (length a))
			(dotimes (j (length b))
				(setf (elt result (+ i j))
							(+ (elt result (+ i j))
								 (* (elt a i) (elt b j))))))
		result))

(defun poly-from-roots (roots)
	(if (= (length roots) 1)
			(list (* -1 (first roots)) 1)
			(poly-mult (poly-from-roots (rest roots))
								 (list (* -1 (first roots)) 1))))

(defun g-random (group)
	(make-instance 'group-element
								 :value (1+ (random (1- (group-order group))))
								 :group group))

(defun pk-gen (crypto-group)
	(let* ((secret-key (g-random crypto-group))
				 (public-key (g^ (make-instance 'group-element
																				:value (group-generator crypto-group)
																				:group crypto-group)
												 secret-key)))
		(list secret-key public-key)))

(defclass cipher ()
	((c1 :accessor cipher-c1
		   :initform (error "You must specify c1 when creating new cipher!")
			 :initarg :c1)
	 (c2 :accessor cipher-c2
		   :initform (error "You must specify c2 when creating new cipher!")
			 :initarg :c2)
	 (public-key :accessor cipher-public-key
							 :initform (error "You must specify the public-key when creating new cipher!")
							 :initarg :pk)))

(defmethod print-object ((object cipher) stream)
	(format stream "(cipher ~d ~d)"
					(cipher-c1 object)
					(cipher-c2 object)))

(defmethod g+ ((a cipher) (b cipher))
	(make-instance 'cipher
								 :c1 (g* (cipher-c1 a)
												 (cipher-c1 b))
								 :c2 (g* (cipher-c2 a)
												 (cipher-c2 b))
								 :pk (cipher-public-key a)))

(defmethod g+ ((a cipher) (b number))
	(let* ((crypto-group (elem-group (cipher-public-key a)))
				 (g (make-instance 'group-element
													 :value (group-generator crypto-group)
													 :group crypto-group)))
		(make-instance 'cipher
									 :c1 (cipher-c1 a)
									 :c2 (g* (cipher-c2 a)
													 (g^ g b))
									 :pk (cipher-public-key a))))

(defmethod g* ((a cipher) (b number))
	(let ((crypto-group (elem-group (cipher-public-key a))))
		(make-instance 'cipher
									 :c1 (g^ (cipher-c1 a)
													 (mod b (group-order crypto-group)))
									 :c2 (g^ (cipher-c2 a)
													 (mod b (group-order crypto-group)))
									 :pk (cipher-public-key a))))

(defmethod g* ((a cipher) (b cipher))
	(let ((crypto-group (elem-group (cipher-public-key a))))
		(make-instance 'cipher
									 :c1 (g^ (cipher-c1 a)
													 (mod b (group-order crypto-group)))
									 :c2 (g^ (cipher-c2 a)
													 (mod b (group-order crypto-group)))
									 :pk (cipher-public-key a))))

(defun pk-find-next-prime (primes &optional current)
	(let ((current-number (if (null current)
														(first (last primes))
														current)))
		(if (not (find-if (lambda (a) (zerop (mod current-number a))) primes))
				current-number
				(pk-find-next-prime primes (+ current-number 2)))))

(defun pk-generate-small-primes (number-of-primes)
	(if (<= number-of-primes 2)
			(list 2 3)
			(let ((current-primes (pk-generate-small-primes (1- number-of-primes))))
				(append current-primes
								(list (pk-find-next-prime current-primes))))))

(defvar *small-primes* (pk-generate-small-primes 100))

(defun fermat-prime-p (n iterations)
	(or (<= iterations 0)
			(let* ((g (make-instance 'group :order n :generator 1))
						 (r (make-instance 'group-element
															 :value (1+ (random (- n 2)))
															 :group g)))
				(and (= (elem-value (g^ r (1- n))) 1)
						 (fermat-prime-p n (1- iterations))))))

(defun pk-prime-p (n &optional small-primes)
	(if (and (not (null small-primes))
					 (find-if (lambda (a) (zerop (mod n a))) small-primes))
			nil
			(fermat-prime-p n 30)))

(defun pk-generate-prime (bit-size)
	(let* ((min (expt 2 (1- bit-size)))
				 (r (+ min (random (1- min)))))
		(if (pk-prime-p r)
				r
				(pk-generate-prime bit-size))))

(defun pk-phi (n)
	(let ((result 0))
		(dotimes (k n)
			(if (= (ggcd n k) 1)
					(setq result (1+ result))))
		result))


(defgeneric pk-enc (key m))
(defgeneric pk-dec (key c))

(defmethod pk-enc ((key group-element) (m number))
	(let ((g (make-instance 'group-element
													:value (group-generator (elem-group key))
													:group (elem-group key)))
				(y (g-random (elem-group key))))
		(make-instance 'cipher
									 :c1 (g^ g y)
									 :c2 (g* (g^ key y) (g^ g m))
									 :pk key)))

(defmethod pk-enc ((key group-element) (m sequence))
	(map (type-of m)
			 (lambda (m-bit)
				 (pk-enc key m-bit))
			 m))

(defmethod pk-dec ((key group-element) (c cipher))
	(let* ((c1 (cipher-c1 c))
				 (c2 (cipher-c2 c))
				 (s (g^ c1 key))
				 (s^-1 (g^ s -1)))
	(g* c2 s^-1)))

(defmethod pk-dec ((key group-element) (cs sequence))
	(map (type-of cs)
			 (lambda (c)
				 (pk-dec key c))
			 cs))

(defun from-peer (public-key original-set)
	(pk-enc public-key (poly-from-roots original-set)))

(defun to-peer (public-key poly-from-peer original-set)
	(let ((crypto-group (elem-group public-key)))
		(map (type-of original-set)
				 (lambda (e)
					 (g+ (g* (poly-eval poly-from-peer e)
									 (elem-value (g-random crypto-group)))
							 e))
				 original-set)))

(defun peer-intersection (private-key evaluated-set original-set)
	(let* ((crypto-group (elem-group private-key))
				 (g (make-instance 'group-element
													 :value (group-generator crypto-group)
													 :group crypto-group))
				 (decrypted-evaluated-set (pk-dec private-key evaluated-set))
				 (local-set (mapcar
										 (lambda (local-elem)
											 (list (g^ g (mod local-elem
																				(group-order crypto-group)))
														 local-elem)) original-set))
				 (isect (remove-if (lambda (e)
														 (not (find-if (lambda (se) (= (elem-value (first e))
																													 (elem-value se)))
																					 decrypted-evaluated-set))) local-set)))
	  (mapcar (lambda (e) (second e)) isect)))

(defun test-psi ()
	(let* ((client-set (list 2 4 6 8 10 12 14))
				 (server-set (list 3 6 9 12))
				 (crypto-group (make-instance 'group :generator 3 :order 2250635938))
				 (keys (pk-gen crypto-group))
				 (fp (from-peer (second keys) client-set))
				 (tp (to-peer (second keys) fp server-set)))
		(peer-intersection (first keys) tp client-set)))

(test-psi)
