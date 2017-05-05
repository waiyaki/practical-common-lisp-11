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
  (values
   (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t 'unsure))
   score))

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


;;;; Testing the filter
(defun add-file-to-corpus (filename type corpus)
  "Add a file to the corpus of messages of known types"
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0)
  "Vector with a fill pointer to hold corpus of messages")

(defun add-directory-to-corpus (dir type corpus)
  "Add all files in a directory in a folder as the same type"
  ;; Using `list-directory` from `pathnames` (Chapter 15)
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defun test-classifier (corpus testing-fraction)
  "Test the classifier by training it on a part of the corpus and having it classify the rest"
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))


(defparameter *max-chars* (* 10 1024)
  "Maximum number of characters to read from a message")

(defun train-from-corpus (corpus &key (start 0) end)
  "Loop over a subsequence of the corpus and use messages in it to train the classifier"
  (loop for index from start below (or end (length corpus)) do
       (destructuring-bind (file type) (aref corpus index)
         (train (start-of-file file *max-chars*) type))))

;;; Test the classifier with messages in a corpus
;;; Return a list containing the classification, score and a list of
;;; the filename, actual type, type returned by classifier and the score.
;;; This information will be used to analyze the results after the fact.
(defun test-from-corpus (corpus &key (start 0) end)
  (loop for index from start below (or end (length corpus)) collect
       (destructuring-bind (file type) (aref corpus index)
         (multiple-value-bind (classification score)
             (classify (start-of-file file *max-chars*))
           (list
            :file file
            :type type
            :classification classification
            :score score)))))


;;;; Utility functions
(defun nshuffle-vector (vector)
  "Shuffle a vector using the Fisher-Yates algorithm. Destructive."
  (loop for index downfrom (1- (length vector)) to 1
     for other = (random (1+ index))
     do (unless (= index other)
          (rotatef (aref vector index) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  "Shuffle vector using the Fisher-Yates algorithm. Non-destructive"
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  "Read and return a substring with first max-chars of a file"
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))


;;;; Analyzing Results
(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
         (ham 'correct)
         (spam 'false-positive)
         (unsure 'missed-ham)))
      (spam
       (ecase classification
         (ham 'false-negative)
         (spam 'correct)
         (unsure 'missed-spam))))))

;;; Predicates for each type of result
(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

;;; Print out the result summary
(defun analyze-results (results)
  (let* ((keys '(total correct false-positive
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
       for (label . count) in counts
       do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                  label count (* 100 (/ count total))))))

;;; Analyze why a message was classified the way it was
(defun explain-classification (file)
  (let* ((text (start-of-file file *max-chars*))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))
