(in-package :agol.core)

(defparameter *grid-width* 150)
(defparameter *grid-height* 100)
(defparameter *grid-wraps-around-p* t "if true, the grid wraps around at the edges")
(defparameter *mutation-rate* 0.0)
(defparameter *enable-colors* t)


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
    :type (array cell-t))
   (width
    :initarg :width
    :initform (error "Must supply width")
    :type fixnum)
   (height
    :initarg :height
    :initform (error "Must supply height")
    :type fixnuma)))

(s:define-do-macro do-cells (((cells x y) grid &optional return) &body body)
  `(let ((,cells (slot-value ,grid 'cells)))
     (dotimes (,y (the fixnum (slot-value ,grid 'height)))
       (dotimes (,x (the fixnum (slot-value ,grid 'width)))
         ,@body))))

(defun make-grid (width height &key (cell-constructor #'make-cell))
  (s:lret ((grid (make-instance 'grid :cells (make-array (list width height) :element-type 'cell-t :initial-element 0) :width width :height height)))
    (do-cells ((cells x y) grid)
      (setf (aref cells x y) (funcall cell-constructor)))))

(s:define-do-macro do-neighbours (((cell nx ny) grid x y &optional return) &body body)
  "Iterate over the neighbours of the cell at `X' and `Y' in `GRID' and evaluate `BODY' for each one.
   For cells on the fringe of the grid, neighbours that would be off the grid are ignored.
  "
  (a:with-gensyms (dx dy)
    `(with-slots (width height) ,grid
       (dotimes (,dx 3)
         (dotimes (,dy 3)
           (let ((,nx (+ ,x ,dx -1))
                 (,ny (+ ,y ,dy -1)))

             (when *grid-wraps-around-p*
               (setf ,nx (mod ,nx width)
                     ,ny (mod ,ny height)))

             (when (and (>= ,nx 0) (< ,nx width)
                        (>= ,ny 0) (< ,ny height))
               (let ((,cell (aref ,grid ,nx ,ny)))
                 ,@body))))))))

(-> count-live-neighbours (grid fixnum fixnum) fixnum)
(defun count-live-neighbours (grid x y)
  "Count the number of live neighbours of the cell at `X' and `Y' in `GRID'"
  (declare (optimize (speed 3) (safety 0)))
  (s:lret ((count (the fixnum 0)))
    (do-neighbours ((neighbour nx ny) grid x y)
      (when (cell-alive-p neighbour)
        (incf count)))))

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
    :type (unsigned-byte 64))))

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
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (live-grid update-grid generation) state
    (let ((update-cells (slot-value update-grid 'cells)))
      (do-cells ((cells x y) live-grid)
        (let ((cell (aref cells x y))
              (live-neighbours (count-live-neighbours live-grid x y)))
          (setf (aref update-cells x y)
                (if (cell-alive-p cell)
                    (if (or (= live-neighbours 2)
                            (= live-neighbours 3))
                        (update-cell cell t)
                        (update-cell cell nil))
                    (if (= live-neighbours 3)
                        (update-cell cell t)
                        (update-cell cell nil)))))))
    (incf generation)
    (rotatef live-grid update-grid)
    state))
