(in-package :agol.core)

;;; Cells are 8-bit unsigned integers
;;; The bit pattern is
;;; The zeroth bit is 1 if the cell is alive, 0 if it is dead
;;; The next three bits encode color (0-7)

(deftype cell-t () '(unsigned-byte 8))
(deftype color-t () '(integer 0 7))

(s:defconst +color-black+ 0)
(s:defconst +color-red+ 1)
(s:defconst +color-orange+ 2)
(s:defconst +color-yellow+ 3)
(s:defconst +color-green+ 4)
(s:defconst +color-blue+ 5)
(s:defconst +color-indigo+ 6)
(s:defconst +color-violet+ 7)

(-> make-cell (&key (:alive-p boolean) (:color color-t)) cell-t)
(defun make-cell (&key (alive-p nil) (color +color-black+))
  "Create a new cell.  If `ALIVE-P' is true, the cell is alive, otherwise it is dead.
  `COLOR' is the color of the cell and must be an integer between 0 and 7."
  (let ((cell 0))
    (setf (ldb (byte 1 0) cell) (if alive-p 1 0))
    (setf (ldb (byte 3 1) cell) color)
    cell))

(-> make-random-cell () cell-t)
(defun make-random-cell ()
  "Create a new cell with a random state and color"
  (make-cell :alive-p (plusp (random 2))
             :color (random-color)))

(-> cell-alive-p (cell-t) boolean)
(defun cell-alive-p (cell)
  "Return true if the `CELL' is alive, false otherwise"
  (s:true (ldb-test (byte 1 0) cell)))

(-> cell-dead-p (cell-t) boolean)
(defun cell-dead-p (cell)
  "Return true if the `CELL' is dead, false otherwise"
  (not (cell-alive-p cell)))

(-> cell-color (cell-t) color-t)
(defun cell-color (cell)
  "Return the color of the `CELL'"
  (ldb (byte 3 1) cell))

(-> random-color () color-t)
(defun random-color ()
  "Return a random color"
  (random 8))

(-> update-cell (cell-t boolean &key (:color (or null color-t))) cell-t)
(defun update-cell (cell alive-p &key (color nil))
  "Update the state of the `CELL' to `ALIVE-P' and (optionally) `COLOR'"
  (setf (ldb (byte 1 0) cell) (if alive-p 1 0))
  (when color
    (setf (ldb (byte 3 1) cell) color))
  cell)

(defclass grid ()
  ((cells
    :initarg :cells
    :initform (error "Must supply cells")
    :type (array cell-t *))
   (width
    :initarg :width
    :initform (error "Must supply width")
    :type (integer 1 *))
   (height
    :initarg :height
    :initform (error "Must supply height")
    :type (integer 1 *))))

;; (defun make-grid (width height &key (cell-constructor #'make-cell))
;;   "Create a new grid of `WIDTH' and `HEIGHT' using `CELL-CONSTRUCTOR' to create the cells"
;;   (let* ((array-size (* width height))
;;          (cells (make-array array-size :element-type 'cell-t :initial-element 0))
;;          (grid  (make-instance 'grid :cells cells :width width :height height)))
;;     (prog1 grid
;;       (do-cells ((cell x y) grid)
;;         (declare (ignorable cell))
;;         (setf (gref grid x y) (funcall cell-constructor))))))

(s:define-do-macro do-cells (((cell x y) grid &optional return) &body body)
  "Iterate over all the cells in `GRID' and evaluate `BODY' for each one.
   `X' and `Y' are the coordinates of the cell.

   The grid is traversed row by row, left to right, top to bottom.
  "
  `(with-slots (width height) ,grid
     (dotimes (,y height)
       (dotimes (,x width)
         (let ((,cell (gref ,grid ,x ,y)))
           ,@body)))))

(defun make-grid (width height &key (cell-constructor #'make-cell))
  (s:lret* ((cells (make-array (* width height) :element-type 'cell-t :initial-element 0))
            (grid (make-instance 'grid :cells cells :width width :height height)))
    (do-cells ((cell x y) grid)
      (declare (ignorable cell))
      (setf (gref grid x y) (funcall cell-constructor)))))

(defun compute-index (grid x y)
  "Compute the index of the cell at `X' and `Y' in a grid of `WIDTH'"
  (with-slots (width height) grid
    (assert (and (>= x 0) (< x width)
                 (>= y 0) (< y height)))
    (+ (* y width) x)))

(defun gref (grid x y)
  "Return the cell at `X' and `Y' in `GRID'"
  (with-slots (cells) grid
    (aref cells (compute-index grid x y))))

(defun (setf gref) (new-value grid x y)
  "Set the cell at `X' and `Y' in `GRID' to `NEW-VALUE'"
  (with-slots (cells) grid
    (setf (aref cells (compute-index grid x y)) new-value)))

(s:define-do-macro do-neighbours (((cell nx ny) grid x y &optional return) &body body)
  "Iterate over the neighbours of the cell at `X' and `Y' in `GRID' and evaluate `BODY' for each one.
   For cells on the fringe of the grid, neighbours that would be off the grid are ignored.
  "
  `(with-slots (width height) ,grid
     (dotimes (dx 3)
       (dotimes (dy 3)
         (let ((,nx (+ x dx -1))
               (,ny (+ y dy -1)))
           (when (and (>= ,nx 0) (< ,nx width)
                      (>= ,ny 0) (< ,ny height))
             (let ((,cell (gref ,grid ,nx ,ny)))
               ,@body)))))))

(defun count-live-neighbours (grid x y)
  "Count the number of live neighbours of the cell at `X' and `Y' in `GRID'"
  (let ((count 0))
    (do-neighbours ((neighbour nx ny) grid x y)
      (declare (ignorable nx ny))
      (when (cell-alive-p neighbour)
        (incf count)))
    count))

(defclass state ()
  ((live-grid
    :reader live-grid
    :initarg :live-grid
    :initform (error "Must supply cells")
    :type grid)
   (update-grid
    :initarg :update-grid
    :initform (error "Must supply cells")
    :type grid)
   (generation
    :initarg :generation
    :initform 1
    :type (integer 1 *))))

(defun initial-state (width height)
  "Create the initial game state with a board of `WIDTH' and `HEIGHT'.
   The board is populated with random cells using `MAKE-RANDOM-CELL'"
  (make-instance 'state
                 :live-grid (make-grid width height :cell-constructor #'make-random-cell)
                 :update-grid (make-grid width height)))


;;; Rules to create the next generation
;;;
;;; 1. If the cell is alive, then it stays alive if it has either 2 or 3 live neighbors
;;; 2. If the cell is dead, then it springs to life only in the case that it has 3 live neighbors
(defun next-generation (state)
  (with-slots (live-grid update-grid generation) state
    (do-cells ((cell x y) live-grid)
      (declare (ignorable x y))
      (let ((live-neighbours (count-live-neighbours live-grid x y)))
        (setf (gref update-grid x y)
              (if (cell-alive-p cell)
                  (if (or (= live-neighbours 2)
                          (= live-neighbours 3))
                      (update-cell cell t)
                      (update-cell cell nil))
                  (if (= live-neighbours 3)
                      (update-cell cell t)
                      (update-cell cell nil))))))
    (incf generation)
    (rotatef live-grid update-grid)
    state))
