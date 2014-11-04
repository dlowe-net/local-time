(in-package #:local-time)

(defmacro defstates (&rest states)
  `(progn
     ,@(loop for i from 0 below (length states)
             for state in states
             collect `(defconstant ,state ,i))))

(defstates
  +START+
  +d+
  +dd+
  +ddd+
  +dddd+
  +m+
  +mm+
  +mmm+
  +mmmm+
  +y+
  +yy+
  +yyy+
  +yyyy+
  +h+
  +hh+
  +cH+
  +cHcH+
  +cM+
  +cMcM+
  +s+
  +ss+
  +l+
  +cL+
  +u+
  +t+
  +tt+
  +cT+
  +cTcT+
  +z+
  +cZ+
  +o+
  +cO+
  +cS+
  +ESCAPE+
  +LITERAL-START+
  +LITERAL+
  +LITERAL-END+)

;;adopted from fast-http
;;#####
(defmacro casev (keyform &body clauses)
  (alexandria:once-only (keyform)
    (flet ((get-val (val)
             (cond
               ((eq val 'otherwise) val)
               ((symbolp val) (symbol-value val))
               ((constantp val) val)
               (T (error "CASEV can be used only with variables or constants")))))
      `(ecase ,keyform
         ,@(loop for (val . clause) in clauses
                 if (eq val 'otherwise)
                 collect `(otherwise ,@clause)
                 else if (listp val)
                 collect `((,@(mapcar #'get-val val)) ,@clause)
                 else
                 collect `(,(get-val val) ,@clause))))))

(defmacro tagcasev (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (casev ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                  collect `(otherwise ,@body (go ,end))
                  else
                  collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                append tag
                else if (not (eq tag 'otherwise))
                collect tag
                collect `(progn ,@body
                                (go ,end)))
        ,end)))

(defmacro go-state (state &key (advance t) (set-state t))
  `(locally (declare (optimize (speed 3) (safety 0)))
     ,@(and set-state
            `((setf state ,state)))
     ,(if advance
          '(go :iteration-exit)
          `(go ,state))))
;;#####

(defmacro while (condition &body body)
  `(loop (if (not ,condition)
             (return)
             (progn
               ,@body))))

(defmacro dispatch-cchar (&key (advance t))
  `(case cchar
     (#\d
      (go-state  +d+ :advance ,advance))
     (#\m
      (go-state  +m+ :advance ,advance))
     (#\y
      (go-state  +y+ :advance ,advance))
     (#\h
      (go-state  +h+ :advance ,advance))
     (#\H
      (go-state  +cH+ :advance ,advance))
     (#\M
      (go-state  +cM+ :advance ,advance))
     (#\s
      (go-state  +s+ :advance ,advance))
     (#\l
      (go-state  +l+ :advance ,advance))
     (#\L
      (go-state  +cL+ :advance ,advance))
     (#\u
      (go-state  +u+ :advance ,advance))
     (#\t
      (go-state  +t+ :advance ,advance))
     (#\T
      (go-state  +cT+ :advance ,advance))
     (#\Z
      (go-state  +cZ+ :advance ,advance))
     (#\z
      (go-state  +cZ+ :advance ,advance))
     (#\o
      (go-state  +o+ :advance ,advance))
     (#\O
      (go-state  +cO+ :advance ,advance))
     (#\S
      (go-state  +cS+ :advance ,advance))
     (#\\
      (go-state  +ESCAPE+ :advance ,advance))
     (:eof
      (go-state :eof :advance nil))
     (t
      (go-state +LITERAL+ :advance nil))))

(defmacro next-char ()
  `(if (>= (1+ i) (length string))
       :eof
       (aref string (1+  i))))

(defmacro adv-char ()
  `(if (>= (1+ i) (length string))
       :eof
       (incf i)))

(defmacro current-char ()
  `(if (= i (length string)) :eof (aref string i)))

(defun literal-character-p (char)
  (case char
    (#\d
     nil)
    (#\m
     nil)
    (#\y
     nil)
    (#\h
     nil)
    (#\H
     nil)
    (#\M
     nil)
    (#\s
     nil)
    (#\l
     nil)
    (#\L
     nil)
    (#\u
     nil)
    (#\t
     nil)
    (#\T
     nil)
    (#\Z
     nil)
    (#\z
     nil)
    (#\o
     nil)
    (#\O
     nil)
    (#\S
     nil)
    (#\\
     nil)
    (:eof
     nil)
    (t
     t)))

(defun parse-string-format (string)
  "Parses format string to regular local-time format list.
Exmaple: yyyy-m-d -> ((:year 4) \"-\" :month \"-\" :day).
Escape character is \\"
  (collectors:with-collector-output (add-format-part)
    (let ((current-literal-buffer)
          (state +LITERAL-start+))
      (loop for i from 0 to (length string)
            as cchar = (current-char) do
               (if (eq cchar :eof)
                   (setf state :eof))
               (tagbody
                  (tagcasev state
                    (+start+)
                    (+s+
                     (if (eql (next-char) #\s)
                         (go-state +ss+)
                         (progn
                           (add-format-part :sec)
                           (go-state +literal-start+))))
                    (+ss+
                     (add-format-part '(:sec 2))
                     (go-state +literal-start+))
                    (+cm+
                     (if (eql (next-char) #\M)
                         (go-state +cmcm+)
                         (progn
                           (add-format-part :min)
                           (go-state +literal-start+))))
                    (+cmcm+
                     (add-format-part '(:min 2))
                     (go-state +literal-start+))
                    (+h+
                     (if (eql (next-char) #\h)
                         (go-state +hh+)
                         (progn
                           (add-format-part :hour12)
                           (go-state +literal-start+))))
                    (+hh+
                     (add-format-part '(:hour12 2))
                     (go-state +literal-start+))
                    (+ch+
                     (if (eql (next-char) #\H)
                         (go-state +chch+)
                         (progn
                           (add-format-part :hour)
                           (go-state +literal-start+))))
                    (+chch+
                     (add-format-part '(:hour 2))
                     (go-state +literal-start+))
                    (+d+
                     (if (eql (next-char) #\d)
                         (go-state +dd+)
                         (progn
                           (add-format-part :day)
                           (go-state +literal-start+))))
                    (+dd+
                     (if (eql (next-char) #\d)
                         (go-state +ddd+)
                         (progn
                           (add-format-part '(:day 2))
                           (go-state +literal-start+))))
                    (+ddd+
                     (if (eql (next-char) #\d)
                         (go-state +dddd+)
                         (progn
                           (add-format-part ':short-weekday)
                           (go-state +literal-start+))))
                    (+dddd+
                     (add-format-part ':long-weekday)
                     (go-state +literal-start+))
                    (+m+
                     (if (eql (next-char) #\m)
                         (go-state +mm+)
                         (progn
                           (add-format-part :month)
                           (go-state +literal-start+))))
                    (+mm+
                     (if (eql (next-char) #\m)
                         (go-state +mmm+)
                         (progn
                           (add-format-part '(:month 2))
                           (go-state +literal-start+))))
                    (+mmm+
                     (if (eql (next-char) #\m)
                         (go-state +mmmm+)
                         (progn
                           (add-format-part ':short-month)
                           (go-state +literal-start+))))
                    (+mmmm+
                     (add-format-part ':long-month)
                     (go-state +literal-start+))
                    (+y+
                     (if (eql (next-char) #\y)
                         (go-state +yy+)
                         (progn
                           (go-state +literal+ :advance nil)
                           (go-state +literal-start+))))
                    (+yy+
                     (if (eql (next-char) #\y)
                         (go-state +yyy+)
                         (progn
                           (add-format-part :short-year)
                           (go-state +literal-start+))))
                    (+yyy+
                     (if (eql (next-char) #\y)
                         (go-state +yyyy+)
                         (progn
                           (add-format-part :short-year)
                           (go-state +literal+ :advance nil)
                           (go-state +literal-start+))))
                    (+yyyy+
                     (add-format-part '(:year 4))
                     (go-state +literal-start+))
                    (+l+
                     (add-format-part '(:msec 3))
                     (go-state +literal-start+))
                    (+cL+
                     (add-format-part '(:msec 2))
                     (go-state +literal-start+))
                    (+u+
                     (let ((count 1))
                       (while (eql (next-char) #\u)
                         (incf count)
                         (adv-char))
                       (add-format-part `(:p ,count)))
                     (go-state +literal-start+))
                    (+t+
                     (if (eql (next-char) #\t)
                         (go-state +tt+)
                         (progn
                           (add-format-part :ap)
                           (go-state +literal-start+))))
                    (+cT+
                     (if (eql (next-char) #\T)
                         (go-state +cTct+)
                         (progn
                           (add-format-part :cap)
                           (go-state +literal-start+))))
                    (+tt+
                     (add-format-part :ampm)
                     (go-state +literal-start+))
                    (+cTcT+
                     (add-format-part :campm)
                     (go-state +literal-start+))
                    (+z+
                     (add-format-part :gmt-offset-or-z)
                     (go-state +literal-start+))
                    (+cZ+
                     (add-format-part :timezone)
                     (go-state +literal-start+))
                    (+o+
                     (add-format-part :gmt-offset)
                     (go-state +literal-start+))
                    (+cO+
                     (add-format-part :gmt-offset-hhmm)
                     (go-state +literal-start+))
                    (+cS+
                     (add-format-part :ordinal-day)
                     (go-state +literal-start+))
                    (+literal-start+
                     (if (literal-character-p cchar)
                         (go-state +literal+ :advance nil)
                         (go-state +literal-end+ :advance nil)))
                    (+literal+
                     (unless current-literal-buffer
                       (setf current-literal-buffer (collectors:make-appender)))
                     (funcall current-literal-buffer cchar)
                     (go-state +literal-start+))
                    (+literal-end+
                     (when current-literal-buffer
                       (let ((char-list (funcall current-literal-buffer)))
                         (add-format-part (coerce char-list 'string)))
                       (setf current-literal-buffer nil))
                     (dispatch-cchar :advance nil))
                    (+escape+
                     (go-state +literal+))
                    (:eof
                     (when current-literal-buffer
                       (let ((char-list (funcall current-literal-buffer)))
                         (add-format-part (coerce char-list 'string)))
                       (setf current-literal-buffer nil))
                     (return)))
                :iteration-exit)))))
