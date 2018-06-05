(defstruct SGameData
  width
  height
  board
  neighbours
  )

(defun boardInit (board width height)
  (dotimes (y height)
    (dotimes (x width)
      (setf (aref board y x) (random 2))
      )
    )
  )

(defun display (gameData)
  (let ((width (SGameData-width gameData))
	(height (SGameData-height gameData))
	(board (SGameData-board gameData)))
    (format t "~C[u" #\Esc)
    (dotimes (y height)
      (dotimes (x width)
	(format t "~C"
		(let ((cell (aref board y x)))
		  (cond
		   ((= cell 1) #\*)
		   ((= cell 0) #\Space)
		   )
		  )
		)
	)
      (format t "~C~C" #\return #\linefeed)
      )
    )
  )

(defun cellProcess (cell neighboursCount)
  (cond
   ((= cell 1) (cond
		 ((<= neighboursCount 1) 0)
		 ((>= neighboursCount 4) 0)
		 (t 1)
		 ))
   (t (cond
	   ((= neighboursCount 3) 1)
	   (t 0)
	   ))
   )
  )

(defun boardProcess (gameData)
  (let ((width (SGameData-width gameData))
	(height (SGameData-height gameData))
	(board (SGameData-board gameData))
	(neighbours (SGameData-neighbours gameData)))
    (dotimes (y height)
      (dotimes (x width)
	(setf (aref board y x) (cellProcess (aref board y x) (aref neighbours y x)))
	)
      )
    )
  )

(defun mima (val max)
  (cond
   ((>= val max) 0)
   ((< val 0) (- max 1))
   (t val)
   )
  )

(defun countNeighbours (gameData x y)
  (let ((width (SGameData-width gameData))
	(height (SGameData-height gameData))
	(board (SGameData-board gameData))
	(neighbours (SGameData-neighbours gameData))
	(neighbour (aref (SGameData-neighbours gameData) y x)))
    (setf neighbour 0)
    (if (= (aref board (mima (- y 1) height) (mima (- x 1) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (+ y 0) height) (mima (- x 1) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (+ y 1) height) (mima (- x 1) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (- y 1) height) (mima (+ x 0) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (+ y 1) height) (mima (+ x 0) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (- y 1) height) (mima (+ x 1) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (+ y 0) height) (mima (+ x 1) width)) 1) (incf neighbour 1))
    (if (= (aref board (mima (+ y 1) height) (mima (+ x 1) width)) 1) (incf neighbour 1))
    (setf (aref neighbours y x) neighbour)
    )
  )

(defun neighboursInit (gameData)
  (let ((width (SGameData-width gameData))
	(height (SGameData-height gameData))
	(board (SGameData-board gameData))
	(neighbours (SGameData-neighbours gameData)))
    (dotimes (y height)
      (dotimes (x width)
	(setf (aref neighbours y x) (countNeighbours gameData x y))
	)
      )
    )
  )

(defun main (width height)
  (let ((gameData (make-SGameData
		   :width width
		   :height height
		   :board (make-array (list height width))
		   :neighbours (make-array (list height width)))))
    (format t "~C[2J~C[0;0f~C[s" #\Esc #\Esc #\Esc)
    (boardInit (SGameData-board gameData) (SGameData-width gameData) (SGameData-height gameData))
    (loop
      (neighboursInit gameData)
      (boardProcess gameData)
      (display gameData)
      )
    )
  )
(main 80 40)
