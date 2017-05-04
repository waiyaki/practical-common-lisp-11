(in-package :waiyaki.spam)

(defparameter *max-ham-score* .4
  "Maximum ham (non-spam) score, beyond which a message is considered spam.")

(defparameter *min-spam-score* .6
  "Minimum spam score, above which a message is considered spam.")

(defun classify (text)
  "Take the text of a message. Classify it as `spam`, `ham` or `unsure`."
  (classification (score (extract-features text))))

(defun classification (score)
  "Determine whether a message is either spam, ham or unsure based on it's score"
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure)))

;;; Extract the features of a message.
;;; For each message, extract the words appearing in it.
;;; For each word, keep track of the number of times it's been seen in spam or ham.
(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we've seen this feature in")))

;;; Printing of all objects is implemented in terms of the generic function `print-object`.
;;; Specialize `print-object` on word-feature to make it print out more informatively.
(defmethod print-object ((object word-feature) stream)
  "Make `word-feature` objects print out more informatively."
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defvar *feature-database* (make-hash-table :test #'equal)
  "Database of features to enable easily finding an object representing a certain feature")

(defvar *total-spams* 0
  "Global count of all processed spams")

(defvar *total-hams* 0
  "Global count of all processed hams")

(defun clear-database ()
  "clear the feature database"
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-hams* 0
   *total-spams* 0))

(defun intern-feature (word)
  "look up the `word-feature` object representing a word. create if one does not exist."
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  "extract individual words from message text and return them in a list."
  (delete-duplicates
   ;; ignore words with less than 3 characters.
   (cl-ppcre:all-matches-as-strings "[a-za-z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  "extract words from a text and translate them into word-features"
  (mapcar #'intern-feature (extract-words text)))


;;; train the filter

(defun train (text type)
  "for some text, increment the ham or spam count depending on text type. increment global total of ham or spam processed too."
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  "Take a word-feature and a type and increment the appropriate slot of that word-feature"

  ;; `ecase` is like the case (switch-case) statement in Algol-derived languages.
  ;; Keys are not evaluated. Meaning `ham` and `spam` are symbols 'ham and 'spam.
  ;; `spam-count` and `ham-count` are defined as accessors, which are setfable, hence the incf.
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  "Increment the total count of `type` of message processed."
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))


;;; Per Word Stats

(defun spam-probability (feature)
  "Compute basic probability that a message containing a given feature is spam."
  (with-slots (spam-count ham-count) feature
    ;; use `max 1` to ensure we don't end up dividing by zero.
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))


(defun bayesian-spam-probability (feature &optional
                                            (assumed-probability 1/2)
                                            (weight 1))
  "Calculate probability factoring in number of datapoints that go into each feature's probability"
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

;;; Combining Probabilities.
(defun score (features)
  "Build two lists of spam and ham probabilities from a list of features"
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      ;; Use untrained-p to skip over features that were not seen during training
      ;; These have 0 ham and spam counts.
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    ;; A low fisher score indicates many hammy features.
    ;; Subtracting it from 1 gives probability that message is ham.
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))


(defun untrained-p (feature)
  "Return true if a feature has zero spam and ham counts"
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))


(defun fisher (probs number-of-probs)
  "The fisher computation described by Robinson."
  (inverse-chi-square
   (* 2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degree-of-freedom)
  (assert (evenp degree-of-freedom))
  (min
   (loop with m = (/ value 2)
      for i below (/ degree-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))
