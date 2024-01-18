(in-package :agol)

;;; Cells are 8-bit unsigned integers
;;; The bit pattern is
;;; The zeroth bit is 1 if the cell is alive, 0 if it is dead
;;; The next three bits encode color (0-7)
(deftype cell-t () '(unsigned-byte 8))
(deftype color-t () '(integer 0 7))

(s:defconst +color-yellow+ 0)
(s:defconst +color-green+ 1)
(s:defconst +color-blue+ 2)
(s:defconst +color-red-violet+ 3)

;; complements
(s:defconst +color-violet+ 4)
(s:defconst +color-red+ 5)
(s:defconst +color-orange+ 6)
(s:defconst +color-yellow-green+ 7)

(-> make-cell (&key (:alive-p boolean) (:color (or null color-t))) cell-t)
(defun make-cell (&key (alive-p nil) (color nil))
  "Create a new cell.  If `ALIVE-P' is true, the cell is alive, otherwise it is dead.
  `COLOR' is the color of the cell and must be an integer between 0 and 7."
  (let ((cell 0))
    (setf (ldb (byte 1 0) cell) (if alive-p 1 0))
    (when color
      (setf (ldb (byte 3 1) cell) color))
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

(defun complement-of (color)
  "Return the complement of `COLOR'"
  (mod (+ color 3) 7))

(-> change-cell (cell-t  &key (:alive-p boolean) (:color (or null color-t))) cell-t)
(defun change-cell (cell &key (alive-p nil) (color nil))
  "Update the state of the `CELL' to `ALIVE-P' and (optionally) `COLOR'"
  (setf (ldb (byte 1 0) cell) (if alive-p 1 0))
  (when color (setf (ldb (byte 3 1) cell) color))
  cell)

(defclass grid ()
  ((cells
    :initarg :cells
    :initform (error "Must supply cells")
    :type (array cell-t))
   (rows
    :initarg :rows
    :initform (error "Must supply rows")
    :type (integer 0 *))
   (columns
    :initarg :columns
    :initform (error "Must supply columns")
    :type (integer 0 *))
   (wraps-around-p
    :initarg :wraps-around-p
    :initform nil
    :type boolean)))

(s:define-do-macro do-cells (((cells row column) grid &optional return) &body body)
  `(let ((,cells (slot-value ,grid 'cells)))
     (with-slots (rows columns) ,grid
       (loop :for ,row :from 0 :below rows
             :do (loop :for ,column :from 0 :below columns
                       :do (progn ,@body))))))

(defmethod print-object ((grid grid) stream)
  (with-slots (rows columns cells) grid
    (format stream "#<GRID ~A x ~A~%~{~{~3,A~^ ~}~^~%~}~%>"
            rows columns
            (loop for row below rows
                  collect (loop for column below columns
                                collect (aref cells row column))))))

(defun make-grid (rows columns &key (cell-constructor #'make-cell) wraps-around-p )
  (let ((grid (make-instance 'grid
                             :wraps-around-p wraps-around-p
                             :cells (make-array (list rows columns) :element-type 'cell-t :initial-element 0) :columns columns :rows rows)))
    (prog1 grid
      (do-cells ((cells row column) grid)
        (setf (aref cells row column) (funcall cell-constructor))))))

(s:define-do-macro do-neighbours (((cell neighbour-row neighbour-column) grid row column &optional return) &body body)
  "Iterate over the neighbours of the cell at `ROW' and `COLUMN' in `GRID' and evaluate `BODY' for each one.
   For cells on the fringe of the grid, neighbours that would be off the grid are ignored.
  "
  (a:with-gensyms (nrow ncolumn)
    `(with-slots (rows columns wraps-around-p cells) ,grid
       (loop :for row-offset from -1 :to 1
             :do (loop :for column-offset :from -1 :to 1
                       :do (let ((,nrow (+ ,row row-offset))
                                 (,ncolumn (+ ,column column-offset)))
                             (unless (and (= ,nrow ,row) (= ,ncolumn ,column))
                               (when wraps-around-p
                                 (setf ,nrow (mod ,nrow rows))
                                 (setf ,ncolumn (mod ,ncolumn columns)))
                               (when (and (>= ,nrow 0)
                                          (>= ,ncolumn 0)
                                          (< ,nrow rows)
                                          (< ,ncolumn columns))
                                 (let ((,cell (aref cells ,nrow ,ncolumn)))
                                   ,@body)))))))))

(defun count-live-neighbours (grid row column)
  "Count the number of live neighbours of the cell at `ROW' and `COLUMN' in `GRID'"
  (length (live-neighbours grid row column)))

(defun live-neighbours (grid row column)
  "Return a list of the live neighbours of the cell at `ROW' and `COLUMN' in `GRID'"
  (let ((neighbours (list)))
    (do-neighbours ((neighbour nrow ncolumn) grid row column)
      (when (cell-alive-p neighbour)
        (push neighbour neighbours)))
    neighbours))

(defun count-live-cells (grid)
  "Count the number of live cells in `GRID'"
  (let ((count 0))
    (do-cells ((cells row column) grid)
      (when (cell-alive-p (aref cells row column))
        (incf count)))
    count))


(defclass state ()
  ((enable-colors-p
    :reader enable-colors-p
    :initarg :enable-colors-p
    :initform *enable-colors*
    :type boolean)
   (live-grid
    :reader live-grid
    :initarg :live-grid
    :initform (error "Must supply cells")
    :type grid)
   (update-grid
    :reader update-grid
    :initarg :update-grid
    :initform (error "Must supply cells")
    :type grid)
   (generation
    :reader generation
    :initarg :generation
    :initform 1
    :type (unsigned-byte 64))))

(defun initial-state (rows columns &key grid-wraps-around-p enable-colors-p)
  "Create the initial game state with a board of `ROWS' and `COLUMNS'.
   The board is populated with random cells using `MAKE-RANDOM-CELL'"
  (make-instance 'state
                 :live-grid (make-grid rows columns :cell-constructor #'make-random-cell :wraps-around-p grid-wraps-around-p)
                 :update-grid (make-grid rows columns :cell-constructor #'make-cell :wraps-around-p grid-wraps-around-p)
                 :enable-colors-p enable-colors-p))

(defun gref (state row column)
  (with-slots (live-grid) state
    (aref (slot-value live-grid 'cells) row column)))

(defun live-cells (state)
  (with-slots (live-grid) state
    (count-live-cells live-grid)))

(defun next-generation (state)
  "Computes the next state and returns two values
   1. The next state
   2. The number of cells that changed state
  "
  (with-slots (live-grid update-grid generation) state
    (let ((update-cells (slot-value update-grid 'cells))
          (changed-cells 0))
      (do-cells ((cells row column) live-grid)
        (let ((cell (aref cells row column))
              (live-neighbours (live-neighbours live-grid row column)))
          (multiple-value-bind (updated-cell changed-p)
              (updated-cell cell live-neighbours)
            (when changed-p
              (incf changed-cells))
            (setf (aref update-cells row column) updated-cell))))
      (incf generation)
      (rotatef live-grid update-grid)
      (values state changed-cells))))

;;; 1. If the cell is alive, then it stays alive if it has either 2 or 3 live neighbors
;;; 2. If the cell is dead, then it springs to life only in the case that it has 3 live neighbors
(defun updated-cell (cell live-neighbours &optional enable-colors-p)
  "Update the state of the `CELL' based on the number of `LIVE-NEIGHBOURS'
   Returns two values
   1. The updated cell
   2. True if the cell changed state, false otherwise
  "
  (let ((alive-neighbours (length live-neighbours)))
    (cond
      ((and (cell-alive-p cell) (not (or (= alive-neighbours 2) (= alive-neighbours 3))))
       (values (change-cell cell :alive-p nil) t))
      ((and (cell-dead-p cell) (= alive-neighbours 3))
       (values (change-cell cell :alive-p t :color (and enable-colors-p (compute-color live-neighbours))) t))
      (t (values cell nil)))))

(defun compute-color (live-neighbours)
  "Compute the color of a cell based on the `LIVE-NEIGHBOURS'
   The following rules apply:
   1. If two parents have the same color (i.e. they form a majority), the child has the same color. (This is the same as Life and Immigration).
   2. If two parents have opposite colors, they cancel, and the child has the same color as the third parent.
   3. Otherwise, if one parent has the opposite type from the other two, the child inherits its color.
   4. Otherwise, when all three parents have the same type, the child is of the opposite type of the fourth color. (This is similar to, but quite different than Quad-Life)
  "
  (let ((colors (mapcar #'cell-color live-neighbouers))
        (distinct-colors (remove-duplicates colors)))
    (cond
      ;; all the same
      ((= (length distinct-colors) 1)
       (complement-of +color-violet+))
      ;; two the same
      ((= (length distinct-colors) 2)
       (if (= (first distinct-colors) (complement-of (second distinct-colors)))
           (second distinct-colors)
           (first distinct-colors)))
      ;; all different
      ((any (lambda (color) (member (complement-of color) distinct-colors)) distinct-colors)
       (complement-of (first distinct-colors))))))
