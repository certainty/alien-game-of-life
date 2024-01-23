(in-package :agol)

(defclass cell ()
  ((alive-p
    :reader cell-alive-p
    :initarg :alive-p
    :initform nil
    :type boolean)
   (color
    :reader cell-color
    :initarg :color
    :initform nil
    :type (or null cl-colors2::rgb))))

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t :identity t)
    (with-slots (alive-p color) cell
      (format stream "~A ~A" (if alive-p "ALIVE" "DEAD") color))))

(defun make-cell (&optional alive-p (color (cl-colors2:as-rgb 0)))
  (make-instance 'cell :alive-p alive-p :color color))

(defun make-random-cell ()
  "Create a new cell with a random state and color"
  (make-cell (plusp (random 2)) (random-color)))

(-> cell-dead-p (cell) boolean)
(defun cell-dead-p (cell)
  "Return true if the `CELL' is dead, false otherwise"
  (not (cell-alive-p cell)))

(-> random-color () color-t)
(defun random-color ()
  "Return a random color"
  (cl-colors2:as-rgb (random 16777216)))

(defun complement-of (color)
  "Return the complement of `COLOR'"
  (mod (+ color 3) 7))

(defun mix-colors (a b)
  "Mix the colors `A' and `B'"
  (cl-colors2:rgb-combination a b 0.7))

(defun kill-cell (cell)
  "Kill the `CELL'"
  (prog1 cell
    (with-slots (alive-p) cell
      (setf alive-p nil))))

(defun resurrect-cell (cell &optional new-color)
  "Resurrect the `CELL'"
  (prog1 cell
    (with-slots (alive-p color) cell
      (setf alive-p t)
      (when new-color
        (setf color new-color)))))

(defun copy-cell (cell)
  "Create a copy of the `CELL'"
  (make-cell (cell-alive-p cell) (cell-color cell)))

(defclass grid ()
  ((cells
    :initarg :cells
    :initform (error "Must supply cells")
    :type (array cell))
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

(defun make-grid (rows columns &key (cell-constructor #'make-random-cell) wraps-around-p)
  (let ((cells (make-array (list rows columns) :initial-element nil)))
    ;; loop over all array slots and call the cell constructor
    (loop :for row :from 0 :below rows
          :do (loop :for column :from 0 :below columns
                    :do (setf (aref cells row column) (funcall cell-constructor))))
    (make-instance 'grid :cells cells :rows rows :columns columns :wraps-around-p wraps-around-p)))

(defmethod print-object ((grid grid) stream)
  (with-slots (rows columns cells) grid
    (format stream "#<GRID ~A x ~A~%~{~{~3,A~^ ~}~^~%~}~%>"
            rows columns
            (loop for row below rows
                  collect (loop for column below columns
                                collect (aref cells row column))))))

(s:define-do-macro do-cells (((cell row column) grid &optional return) &body body)
  (a:with-gensyms (cells)
    `(let ((,cells (slot-value ,grid 'cells)))
       (with-slots (rows columns) ,grid
         (loop :for ,row :from 0 :below rows
               :do (loop :for ,column :from 0 :below columns
                         :do
                            (let ((,cell (aref ,cells ,row ,column)))
                              (progn ,@body))))))))

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

(defun copy-grid (grid)
  "Create a copy of the `GRID'"
  (with-slots (rows columns cells wraps-around-p) grid
    (let ((new-cells (make-array (list rows columns) :initial-element nil)))
      (do-cells ((cell row column) grid)
        (setf (aref new-cells row column) (copy-cell cell)))
      (make-instance 'grid :cells new-cells :rows rows :columns columns :wraps-around-p wraps-around-p))))

(defun living-neighbours (grid row column)
  "Return a list of the live neighbours of the cell at `ROW' and `COLUMN' in `GRID'"
  (let ((neighbours (list)))
    (do-neighbours ((neighbour nrow ncolumn) grid row column)
      (when (cell-alive-p neighbour)
        (push neighbour neighbours)))
    neighbours))

(defclass state ()
  ((enable-colors-p
    :reader enable-colors-p
    :initarg :enable-colors-p
    :initform *enable-colors*
    :type boolean)
   (mutation-rate
    :reader mutation-rate
    :initarg :mutation-rate
    :initform 0
    :type (real 0 1))
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

(defun make-state (rows columns &key grid-wraps-around-p enable-colors-p (mutation-rate 0))
  "Create the initial game state with a board of `ROWS' and `COLUMNS'.
   The board is populated with random cells using `MAKE-RANDOM-CELL'"
  (let ((live-grid (make-grid rows columns :cell-constructor #'make-random-cell :wraps-around-p grid-wraps-around-p)))
    (make-instance 'state
                   :live-grid live-grid
                   :update-grid (copy-grid live-grid)
                   :enable-colors-p enable-colors-p
                   :mutation-rate mutation-rate)))

(defun gref (grid row column)
  "Get the cell at `ROW' and `COLUMN' in `GRID'"
  (aref (slot-value grid 'cells) row column))

(defsetf gref (grid row column) (new-value)
  `(setf (aref (slot-value ,grid 'cells) ,row ,column) ,new-value))

(defun live-cells (state)
  (with-slots (live-grid) state
    (let ((count 0))
      (do-cells ((cell row column) live-grid)
        (when (cell-alive-p cell)
          (incf count)))
      count)))

;;; 1. If the cell is alive, then it stays alive if it has either 2 or 3 live neighbors
;;; 2. If the cell is dead, then it springs to life only in the case that it has 3 live neighbors
(defun next-generation (state)
  "Updates the state and returns the number of cells that changed"
  (with-slots (live-grid update-grid generation) state
    (let ((changed-cells 0))
      (do-cells ((cell row column) live-grid)
        (let* ((cell-to-update (gref update-grid row column))
               (live-neighbours (living-neighbours live-grid row column))
               (live-neighbours-# (length live-neighbours)))

          ;; FIXME: we should be able to go without this
          (with-slots (alive-p color) cell-to-update
            (setf alive-p (cell-alive-p cell))
            (setf color (cell-color cell)))

          (if (cell-alive-p cell)
              (when (not (or (= live-neighbours-# 2) (= live-neighbours-# 3)))
                (incf changed-cells)
                (kill-cell cell-to-update))
              (when (= live-neighbours-# 3)
                (incf changed-cells)
                (if (enable-colors-p state)
                    (let ((new-color (compute-color live-neighbours)))
                      (when (and (mutation-rate state) (<= (random 1.0) (mutation-rate state)))
                        (setf new-color (random-color)))
                      (resurrect-cell cell-to-update new-color))
                    (resurrect-cell cell-to-update))))))
      (incf generation)
      (rotatef live-grid update-grid)
      changed-cells)))

(defun compute-color (live-neighbours)
  (let* ((colors (mapcar #'cell-color live-neighbours))
         (distinct-colors (remove-duplicates colors)))
    (cond
      ;; all the same
      ((= (length distinct-colors) 1)
       (first distinct-colors))
      ;; two the same
      ((= (length distinct-colors) 2)
       (mix-colors (first distinct-colors) (second distinct-colors)))
      ;; all different
      (t (mix-colors (first distinct-colors) (mix-colors (second distinct-colors) (third distinct-colors)))))))
