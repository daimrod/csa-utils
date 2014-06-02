;;;; csa-utils.lisp

(in-package #:csa-utils)

;;; "csa-utils" goes here. Hacks and glory await!

(defparameter cl-csv:*separator* #\;)

(defun freq-lines (lines &key (key #'identity))
  (iter (with acc = (make-list (length (car lines)) :initial-element 0))
    (for line- in lines)
    (for line = (mapcar #'parse-integer (funcall key line-)))
    (setf acc (mapcar #'+ acc line))
    (finally (return (mapcar (rcurry #'/ (length lines)) acc)))))

(defun read-dat-file (file)
  ;; ignore the first line because it's the headers' title
  (rest
   (with-open-file (input file)
     (cl-csv:read-csv input))))

(defun to-float (line)
  (mapcar (rcurry #'coerce 'float) line))

(defun top-n (list &optional (n 1))
  (mapcar (lambda (pair)
            (list (coerce (first pair) 'float)
                  (to-modality (second pair))))
          (subseq (sort (mapcar #'list list (iota (length list))) #'>
                        :key #'car)
                  0
                  n)))

(defparameter *long-modalities*
  '(established
    previous
    causality
    consensus
    importance
    discovered
    achieved
    improved
    modified
    supported
    hypothesis
    reported
    constructed
    method
    usage
    interest
    new
    differentiated
    associated
    similarity
    difficulty
    future
    uncertainty
    weakness
    criticism
    negation
    emerged))

(defparameter *short-modalities*
  '(established
    prev
    causality
    consensus
    importance
    disc
    achieved
    improved
    modif
    support
    hypo
    report
    constr
    method
    usage
    interest
    new
    differ
    assoc
    simil
    difficult
    future
    uncertain
    weak
    critic
    neg
    emerged))

(defparameter *modalities* *long-modalities*)

(defun to-modality (position)
  (elt *modalities* position))

(defun read-cocit-file (file)
  (iter (with db = (rest
                    (with-open-file (input file)
                      (cl-csv:read-csv input))))
    (with ret = (make-hash-table :test #'equalp))
    (for (pmid1 pmid2 cpa refdoc-id1 refdoc-id2 citing-document) in db)
    (pushnew citing-document (gethash (list pmid1 pmid2) ret (list)) :test #'string=)
    (finally (return ret))))

(defun contexts-from-pair (pmid1 pmid2)
  (iter (for article in (gethash (list pmid1 pmid2) *cocit-db*))
    (appending
     (iter (for line in (remove-if (lambda (line)
                                     (not (and (string= article (first line))
                                               (find-if (lambda (el)
                                                          (or (string= el pmid1)
                                                              (string= el pmid2)))
                                                        (cl-ppcre:split "," (second line))))))
                                   *db*))
       (collect line)))))

(defun log-likelihood-ratio (observed expected)
  "See http://wordhoard.northwestern.edu/userman/analysis-comparewords.html"
  (let ((c (reduce #'+ observed))
        (d (reduce #'+ expected)))
    (iter (for a in observed)
      (for b in expected)
      (for e1 = (* c (/ (+ a b) (+ c d))))
      (for e2 = (* d (/ (+ a b) (+ c d))))
      (collect (* 2 (+ (* a (log (/ a e1)))
                       (* b (log (/ b e2)))))))))
