(package.seed:define-seed-package :tree-walking.seed :export-capitalized t)

(in-package :tree-walking.seed)

(defun Walk-tree-atoms (fn tree)
  (when tree
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  (walk (car tr))
                  (when (cdr tr)
                    (walk (cdr tr))))
                 (t (funcall fn tr)))))
      (walk tree))))

#+self-test.seed
(self-test.seed:define-self-test walk-tree-atoms
  (zerop (let ((y 0)) (walk-tree-atoms (lambda (x)
                                         (declare (ignore x))
                                         (incf y)) '()) y))
  (equal '(3 2 1) (let (a)
                    (walk-tree-atoms (lambda (x) (push x a)) '(1 (2 (3))))
                    a)))

(defun Map-tree-atoms (fn tree)
  (when tree
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  (cons (walk (car tr))
                        (if (cdr tr)
                            (walk (cdr tr))
                            nil)))
                 (t (funcall fn tr)))))
      (walk tree))))

#+self-test.seed
(self-test.seed:define-self-test map-tree-atoms
  (zerop (let ((y 0)) (map-tree-atoms
                       (lambda (x) (declare (ignore x)) (incf y))
                       '())
           y))
  (equal '(1 (nil (2))) (map-tree-atoms #'identity '(1 (nil (2))))))

(defun Walk-tree-conses-preorder (fn tree)
  (when tree
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  (funcall fn tr)
                  (walk (car tr))
                  (walk (cdr tr)))
                 (atom tr))))
      (walk tree))))

#+self-test.seed
(defmacro with (binding &body body)
  "Establish BINDING and return it after BODY."
  `(let (,binding)
     ,@body
     ,(if (symbolp binding)
          binding
          (car binding))))

#+self-test.seed
(self-test.seed:define-self-test walk-tree-conses-preorder
  (zerop (with (y 0)
           (walk-tree-conses-preorder (lambda (x)
                                        (declare (ignore x))
                                        (incf y))
                                      '())))
  (equal '((3) (2 3) (1 2 3))
         (with y
           (walk-tree-conses-preorder
            (lambda (x) (push x y))
            '(1 2 3))))
  (equal '((3) ((3)) (2 (3)) ((2 (3))) (1 (2 (3))))
         (with y
           (walk-tree-conses-preorder
            (lambda (x) (push x y))
            '(1 (2 (3)))))))

(defun Walk-tree-conses-postorder (fn tree)
  (when tree
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  (walk (car tr))
                  (walk (cdr tr))
                  (funcall fn tr))
                 (atom tr))))
      (walk tree))))

#+self-test.seed
(self-test.seed:define-self-test walk-tree-conses-postorder
  (zerop (with (y 0)
           (walk-tree-conses-postorder (lambda (x)
                                         (declare (ignore x))
                                         (incf y))
                                       '())))
  (equal '((1 (2 (3))) ((2 (3))) (2 (3)) ((3)) (3))
         (with y
           (walk-tree-conses-postorder
            (lambda (x) (push x y))
            '(1 (2 (3)))))))

(defun Map-tree-conses-preorder (fn tree)
  (when tree
    (labels ((walk (tr)
               (typecase tr
                 (cons (let ((new (funcall fn tr)))
                         (if new
                             (cons (walk (car new))
                                   (walk (cdr new)))
                             '())))
                 (atom tr))))
      (walk tree))))

#+self-test.seed
(self-test.seed:define-self-test map-tree-conses-preorder
  (zerop (with (y 0)
           (map-tree-conses-preorder
            (lambda (x)
              (declare (ignore x))
              (incf y))
            '())))
  (equal '(1 2 3) (map-tree-conses-preorder #'identity '(1 2 3)))
  (equal '((3) (2 3) (1 2 3))
         (with y
           (map-tree-conses-preorder
            (lambda (x) (push x y) x)
            '(1 2 3))))
  (equal '(1) (map-tree-conses-preorder
               (lambda (x)
                 (if (and (numberp (car x))
                          (evenp (car x)))
                     nil
                     x))
               '(1 2))))

(defun Map-tree-conses-postorder (fn tree)
  (labels ((walk (tr)
             (typecase tr
               (cons (funcall fn (cons (walk (car tr))
                                       (walk (cdr tr)))))
               (atom tr))))
    (walk tree)))

#+self-test.seed
(self-test.seed:define-self-test map-tree-conses-postorder
  (zerop (with (y 0)
           (map-tree-conses-postorder
            (lambda (x)
              (declare (ignore x))
              (incf y))
            '())))
  (equal '(1 2 3) (map-tree-conses-postorder #'identity '(1 2 3)))
  (equal '((1 2 3) (2 3) (3))
         (with y
           (map-tree-conses-postorder
            (lambda (x) (push x y) x)
            '(1 2 3))))
  (equal '(1) (map-tree-conses-postorder
               (lambda (x)
                 (if (and (numberp (car x))
                          (evenp (car x)))
                     nil
                     x))
               '(1 2))))

(defun Map-tree-atoms-circle (fn tree)
  (let ((seen-table (make-hash-table :test 'eq)) ; original cons -> marker
        (value-table (make-hash-table :test 'eq))) ; marker -> walked cons
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  ;; If tr has been seen before, return the
                  ;; corresponding marker.
                  (cond ((gethash tr seen-table))
                        ;; Otherwise mark as seen and store the walked
                        ;; tree in value-table.
                        (t (let ((marker (list nil)))
                             (setf (gethash tr seen-table) marker)
                             (setf (gethash marker value-table)
                                   (cons (walk (car tr))
                                         (walk (cdr tr))))))))
                 (atom (funcall fn tr))))
             (replace-markers (tr)
               (typecase tr
                 (cons
                  (let ((car (gethash (car tr) value-table))
                        (cdr (gethash (cdr tr) value-table)))
                    (if car
                        (rplaca tr car)
                        (replace-markers (car tr)))
                    (if cdr
                        (rplacd tr cdr)
                        (replace-markers (cdr tr)))))
                 (atom nil))))
      (let ((new (walk tree)))
        (replace-markers new)
        new))))

(defun Tree-contains-p (predicate tree)
  (walk-tree-atoms
   (lambda (e)
     (when (funcall predicate e)
       (return-from tree-contains-p t)))
   tree))
