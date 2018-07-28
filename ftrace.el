;;; ftrace.el --- Tools to parse ftrace/dmesg data and plot them

;; Copyright (C) 2018 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 1.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)
(require 'gplot)
(require 'org-table)
(require 'subr-x)

(defstruct fevt			      ;; ftrace event structure
  type					; Event type (sched_switch, ...)
  pid					; PID of the event
  cpu					; CPU of the event
  ts					; Timestamp of the event (in s)
  tsc					; TSC timestamp if present
  irqs-off				; IRQ disabled during that event
  need-resched				; Need resched flag
  hardirq-softirq			; Hard IRQ & Soft IRQ status
  preempt-depth				; Preemption depth
  args)					; Associative list of
					; arguments.  Depend on the
					; Event Type

(defstruct fmeta
  name
  reference
  events
  cache)

(defvar ftrace-database '()
  "List of `fmeta' structures.")

(defvar ftrace-current nil
  "Local stack defined variable.")

(defmacro ftrace-name ()
  "Return `ftrace-current' name."
  `(fmeta-name ftrace-current))

(defmacro ftrace-events ()
  "Return `ftrace-current' events list."
  `(fmeta-events ftrace-current))

(defmacro ftrace-cache ()
  "Return `ftrace-current' cache associative list."
  `(fmeta-cache ftrace-current))

(defun ftrace-parse=-args (args)
  "Convert ARGS string into an associative list.  ARGS must
contains KEY=VAL pairs."
  (let* ((res '())
	 (token (split-string args "=" t " "))
	 (name (intern (car token))))
    (setf token (cdr token))
    (while token
      (let ((pivot (position ? (car token) :from-end t)))
	(let ((value (substring (car token) 0 pivot)))
	  (when (save-match-data (string-match "\\`[0-9][0-9]*\\'" value))
	    (setf value (string-to-number value)))
	  (push (cons name value) res))
	(setf name (intern (substring (car token) (when pivot (1+ pivot))))))
      (setf token (cdr token)))
    (nreverse res)))

(defun ftrace-parse-comma-args (args)
  "Convert ARGS string into an associative list.  ARGS must
contains 'KEY VAL' pairs separated by comma and spaces."
  (let ((res '()))
    (dolist (token (split-string args "," t " "))
      (let* ((pivot (position ? token :from-end t))
	     (value (substring token (1+ pivot))))
	  (if (save-match-data (string-match "\\`[0-9][0-9]*\\'" value))
	      (setf value (string-to-number value))
	    (when (string-prefix-p "0x" value)
	      (setf value (string-to-number (substring value 2) 16))))
	  (push (cons (intern (substring token 0 pivot)) value) res)))
    (nreverse res)))

(defcustom fevent-parser-sets
  '(("Linux Scheduler" . ((sched_switch . ftrace-parse=-args)
			  (sched_wakeup . ftrace-parse=-args))))
  "List of set of events.")

(defsubst tsc-to-s (tsc)
  "Convert TSC timestamp to seconds.  Calibrated for Goldmont
cores."
  (/ (lsh (/ (lsh tsc 6) 1900) -6) 1000000.0))

(defsubst s-to-tsc (s)
  "Convert seconds to TSC timestamp.  Calibrated for Goldmont
cores."
  (lsh (* (lsh (round (* 1000000.0 s)) 6) 1900) -6))

(defsubst ftrace-build-regexp (events)
  "Create a regular expression of EVENTS."
  (regexp-opt (mapcar 'symbol-name (mapcar 'car events)) 'words))

(defsubst dmesg-line-parser (event arg-parser)
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward (concat "\\[ *\\\([0-9]+\\\.?[0-9]+\\\) *\\] .*"
				     (symbol-name event))
			     (line-end-position) t)
      (make-fevt :type event
		 :ts (string-to-number (match-string 1))
		 :args (when arg-parser
		       (funcall arg-parser
				(buffer-substring (match-end 0)
						  (line-end-position))))))))

(defsubst ftrace-line-parser (event arg-parser)
  (let* ((lb (line-beginning-position))
	 (event (intern (match-string 1)))
	 (ts (string-to-number (buffer-substring (+ lb 34) (+ lb 46))))
	 (tsc nil))
    (unless (floatp ts)
      (setf tsc ts)
      (setf ts (tsc-to-s ts)))
    (make-fevt :type event
	       :pid (string-to-number (buffer-substring (+ lb 17) (+ lb 22)))
	       :cpu (string-to-number (buffer-substring (+ lb 24) (+ lb 27)))
	       :ts ts
	       :tsc tsc
	       :irqs-off (char-after (+ lb 29))
	       :need-resched (char-after (+ lb 30))
	       :hardirq-softirq (char-after (+ lb 31))
	       :preempt-depth (- (char-after (+ lb 32)) ?0)
	       :args (when arg-parser
		       (funcall arg-parser
				(buffer-substring (match-end 0)
						  (line-end-position)))))))

(defun ftrace-read-parser-type ()
  (let ((suffix "-line-parser")
	(fun-list '()))
    (mapatoms
     (lambda (x)
       (when (and (symbol-function x)
		  (string-suffix-p suffix (symbol-name x)))
	 (let ((type (propertize (string-remove-suffix suffix (symbol-name x))
				 'fun x)))
	   (if fun-list
	       (push type fun-list)
		      (setf fun-list (list type)))))))
    (get-text-property
     0 'fun (ido-completing-read "Parser Type: " fun-list nil t))))

(defun ftrace-read-event-set ()
  (let* ((events (append '("All") (mapcar 'car fevent-parser-sets)))
	 (set-name (ido-completing-read "Event set: " events)))
    (if (string= set-name "All")
	(apply 'append (mapcar 'cdr fevent-parser-sets))
      (assoc-default set-name fevent-parser-sets))))

(defun ftrace-import-events (file name line-parser events)
  (interactive (list (read-file-name "File: ")
		     (read-string "Name: ")
		     (ftrace-read-parser-type)
		     (ftrace-read-event-set)))
  (let* ((ftrace-current (make-fmeta :name name :reference file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((p (make-progress-reporter "Parsing Events" 0 (point-max)))
	    (re (ftrace-build-regexp events)))
	(while (re-search-forward re nil t)
	  (let ((event (intern (match-string 1))))
	    (push (funcall line-parser event (assoc-default event events))
		  (ftrace-events)))
	  (progress-reporter-update p (point)))
	(progress-reporter-done p)
	(with-temp-message "Sorting Events..."
	  (setf (ftrace-events) (cl-sort (ftrace-events) '< :key 'fevt-ts)))))
    (push ftrace-current ftrace-database)))

(defun fevt-sched-name (pair)
  (format "%s (%d)" (assoc-default 'next_comm (fevt-args (car pair)))
	  (fevt-sched-pid pair)))

(defun fevt-sched-cpu (pair)
  (fevt-cpu (car pair)))

(defun fevt-sched-pid (pair)
  (assoc-default 'next_pid (fevt-args (car pair))))

(defsubst fevt-from (pair)
  (fevt-ts (car pair)))

(defsubst fevt-until (pair)
  (fevt-ts (cdr pair)))

(defun fevt-time-spent (pair)
  (- (fevt-until pair) (fevt-from pair)))

(defun fevt-time-spent-ms (pair)
  (* 1000 (fevt-time-spent pair)))

(defsubst ftrace-read-from-ts ()
  (read-number "From (s): " (apply 'max (ftrace-firsts))))

(defsubst ftrace-read-until-ts ()
  (read-number "Until (s): " (apply 'min (ftrace-lasts))))

(defsubst ftrace-read-pid ()
  (cl-flet ((format-process (pairs)
	      (propertize (format "%s %d" (evt-sched-name (car pairs))
				  (evt-sched-pid (car pairs)))
			  'pid (evt-sched-pid (car pairs)))))
    (get-text-property
     0 'pid (ido-completing-read
	     "Process: " (mapcar #'format-process (ftrace-sched))
	     nil t))))

(defmacro ftrace-defun-cached (name docstring &rest body)
  (declare (doc-string 2) (indent 1))
  (let ((cpl (gensym)))
    `(defun ,name ()
       ,docstring
       (let ((,cpl (assq (quote ,name) (ftrace-cache))))
	 (if ,cpl
	     (cdr ,cpl)
	   (with-temp-message (format "Computing %s..."
				      (symbol-name (quote ,name)))
	     (push (cons (quote ,name) (progn ,@body)) (ftrace-cache)))
	   (assoc-default (quote ,name) (ftrace-cache)))))))

(ftrace-defun-cached ftrace-cpus
  "Computes the sorted list of CPU from the `ftrace-events'."
  (sort (delete-duplicates (mapcar 'fevt-cpu (ftrace-events))) '<))

(ftrace-defun-cached ftrace-firsts
  "Computes the per-CPU first event timestamp list."
  (mapcar 'fevt-ts
	  (mapcar (rcurry 'find (ftrace-events) :key 'fevt-cpu)
		  (ftrace-cpus))))

(ftrace-defun-cached ftrace-lasts
  "Computes the per-CPU latest event timestamp list."
  (mapcar 'fevt-ts
	  (mapcar (rcurry 'find (ftrace-events) :key 'fevt-cpu :from-end t)
		  (ftrace-cpus))))

(ftrace-defun-cached ftrace-sched
  "Compute a per process list of pair of `sched_switch' events."
  (let ((res '())
	(processes (make-hash-table))
	(start (make-vector (1+ (apply 'max (ftrace-cpus))) nil)))
    (dolist (e (ftrace-events))
      (when (eq 'sched_switch (fevt-type e))
	(let ((prev-e (aref start (fevt-cpu e))))
	  (when prev-e
	    (push (cons prev-e e)
		  (gethash (assoc-default 'prev_pid (fevt-args e))
			   processes))))
	(aset start (fevt-cpu e) e)))
    (maphash (lambda (key val) (push (nreverse val) res)) processes)
    (cl-sort res '< :key (lambda (x) (fevt-sched-pid (car x))))))

(ftrace-defun-cached ftrace-types
  "List of events."
  (delete-duplicates (mapcar 'fevt-type (ftrace-events))))

(defun ftrace-sample (data valuef fromf untilf from until period)
  "It samples DATA where DATA is a list of objects.  VALUEF is a
function which return the value of an element of DATA.  FROMF is
a function which return the beginning timestamp of an element of
DATA.  UNTILF is a function which returns the end timestamp of an
element of DATA.  [ FROM - UNTIL ] define the range of timestamp
to take into account in DATA.  PERIOD is the sample period."
  (let* ((nb (1+ (round (/ (- until from) period))))
	 (sample (make-vector nb 0)))
    (dolist (cur data)
      (let* ((index (round (/ (- (funcall fromf cur) from) period)))
	     (to-index (round (/ (- (funcall untilf cur) from) period)))
	     (div (1+ (- to-index index))))
	(while (<= index to-index)
	  (when (and (> index 0) (< index nb))
	    (incf (aref sample index)
		  (/ (funcall valuef cur) div)))
	  (incf index))))
    (mapcar 'identity sample)))

(defun ftrace-frequency (data fromf untilf from until)
  "It computes the list of time between events in the form
'((event-timestamp time-elapsed-since-previous-event) ...).  DATA
is a list events.  FROMF is a function which return the beginning
timestamp of the an element of DATA.  UNTILF is a function which
returns the end timestamp of an element of DATA."
  (let ((res '())
	(cur-until (funcall untilf (car data))))
    (dolist (cur (cdr data))
      (let ((cur-from (funcall fromf cur)))
	(when (and (> cur-from from) (< cur-until until))
	  (push (list cur-from (- cur-from cur-until)) res)))
      (setf cur-until (funcall untilf cur)))
    (nreverse res)))

(defun ftrace-stats (data fromf untilf from until)
  "It computes statistics on the DATA event list.  FROMF is a
function which return the beginning timestamp of an element of
DATA.  UNTILF is a function which returns the end timestamp of an
element of DATA.  [ FROM - UNTIL ] define the range of timestamp
to take into account in DATA.

The statistics are put in a associative list.
- 'min' is the minimum time elapsed between two events.
- 'max' is the maximum time elapsed between two events.
- 'average' is the average time elapsed between two events.
- 'event-per-sec' is the number of event in a second."
  (let* ((freqs (mapcar 'cadr (ftrace-frequency data fromf untilf from until))))
    `((min . ,(apply 'min freqs))
      (max . ,(apply 'max freqs))
      (average . ,(/ (apply '+ freqs) (length freqs)))
      (event-per-sec . ,(/ (length data)
			   (- (funcall untilf (car (last data)))
			      (funcall fromf (car data))))))))

(defun ftrace-read-current (&optional name)
  (find (or name
	    (ido-completing-read "Events database: "
				 (mapcar 'fmeta-name ftrace-database)
				 nil t))
	ftrace-database :key 'fmeta-name :test 'string=))

(defsubst ftrace-cpuload-read-params ()
  (let ((ftrace-current (ftrace-read-current)))
    (list ftrace-current
	  (ftrace-read-from-ts) (ftrace-read-until-ts)
	  (read-number "Sample period (ms): " 100)
	  (read-number "Max process: " 30))))

(defun ftrace-plot-cpuload (ftrace-current from until period limit &optional data title)
  (interactive (ftrace-cpuload-read-params))
  (let ((period-s (/ period 1000.0)))
    (cl-flet ((sample-proc (pair)
	       (cons (fevt-sched-name (car pair))
		     (ftrace-sample pair 'fevt-time-spent-ms 'fevt-from
				    'fevt-until from until period-s))))
      (let* ((sample (with-temp-message "Sampling..."
		       (mapcar #'sample-proc (or data (cdr (ftrace-sched))))))
	     (sorted (with-temp-message "Sorting..."
		       (cl-sort sample '> :key (lambda (x) (apply '+ (cdr x))))))
	     (aggregated `(("*Aggregated*" .
			    ,(apply 'mapcar* '+
				    (mapcar 'cdr (subseq sorted limit))))))
	     (to-plot (nconc (subseq sorted 0 limit) aggregated)))
	(gplot-cumulative (format "%s - Sampled period is %d ms"
				  (or title "CPU Load") period)
			  "Time (s)" "Duration (ms)"
			  (apply 'mapcar* 'list
				 (number-sequence from until period-s)
				 (mapcar 'cdr to-plot))
			  (mapcar 'car to-plot))))))

(defun ftrace-plot-cpuload-for-cpu (ftrace-current from until period limit cpu)
  (interactive (append (ftrace-cpuload-read-params)
		       (list (read-number "CPU: "))))
  (cl-flet ((cpu-filter (cpu sched)
	      (remove-if-not (curry '= cpu) sched
			     :key '(lambda (x) (fevt-cpu (car x))))))
    (ftrace-plot-cpuload ftrace-current from until period limit
			 (delq nil (mapcar (curry #'cpu-filter cpu)
					   (cdr (ftrace-sched))))
			 (format "CPU %d" cpu))))

(defun ftrace-plot-cpuload-per-cpu (ftrace-current from until period limit)
  (interactive (ftrace-cpuload-read-params))
  (with-multiplot (length (ftrace-cpus)) "CPU Load Per CPU"
    (dolist (cpu (ftrace-cpus))
      (ftrace-plot-cpuload-for-cpu ftrace-current from until period limit cpu))))

(defun ftrace-plot-sched-freq (ftrace-current regexp from until)
  (interactive (let ((ftrace-current (ftrace-read-current)))
		 (list ftrace-current
		       (read-regexp "Regexp: ")
		       (ftrace-read-from-ts) (ftrace-read-until-ts))))
  (cl-flet ((filter (p)
	     (string-match-p regexp (fevt-sched-name (car p))))
	    (time-spent (p)
	     (list (fevt-ts (car p)) (fevt-time-spent-ms p)))
	    (in-boundary (x)
	     (and (> (fevt-from x) from) (< (fevt-until x) until)))
	    (sched-freq (p)
	     (mapc (lambda (x) (setcdr x (cons (* 1000 (cadr x)) nil)))
		   (ftrace-frequency p 'fevt-from 'fevt-until from until))))
    (let ((processes (remove-if-not #'filter (ftrace-sched))))
      (with-multiplot (length processes) "Time Spent And Time To Schedule"
	(dolist (p processes)
	  (when (setf p (remove-if-not #'in-boundary p))
	    (gplot-plot (fevt-sched-name (car p)) "Time (s)" "Time (ms)"
			(list (mapcar #'time-spent p) (sched-freq p))
			'(((title . "Time spent") (style . "line"))
			  ((title . "Time to schedule") (style . "line"))))))))))

(defun org-insert-ftrace-sched (sched)
  (let ((head (list (list "End (s)" "Start (s)" "Time to schedule (ms)"
			  "End state" "Start state" "End core" "Start core")
		    'hline))
	(has-tsc (fevt-tsc (caar sched)))
	res)
    (when has-tsc
      (setcdr (car head) (cons "TSC start" (cdar head)))
      (push "TSC end" (car head)))
    (dolist (cur sched)
      (let ((new (list (fevt-from cur) (fevt-until cur)
		       (* 1000 (- (fevt-until cur) (fevt-from cur))))))
	  (setf new (mapcar (curry 'format (if has-tsc "%.03f" "%.06f")) new))
	  (nconc new (list (assoc-default 'prev_state (fevt-args (car cur)))))
	  (nconc new (list (assoc-default 'prev_state (fevt-args (cdr cur)))))
	  (nconc new (list (fevt-cpu (car cur))))
	  (nconc new (list (fevt-cpu (cdr cur))))
	  (when has-tsc
	    (setcdr new (cons (fevt-tsc (cdr cur)) (cdr new)))
	    (push (fevt-tsc (car cur)) new))
	  (push new res)))
    (insert (orgtbl-to-orgtbl (append head (nreverse res))
			      '(:fmt "%s")) "\n")))

(defun org-insert-ftrace-sched-over-limit (ftrace-current pid limit &optional from until)
  (interactive (let ((ftrace-current (ftrace-read-current)))
		 (list ftrace-current
		       (read-number "Process ID: ")
		       (read-number "Frequency (ms): ")
		       (ftrace-read-from-ts) (ftrace-read-until-ts))))
  (let* ((process (find pid (ftrace-sched)
			:key (lambda (x) (fevt-sched-pid (car x)))
			:test '=))
	 sched)
    (let ((cur process))
      (while (cdr cur)
	(when (and (> (fevt-until (car cur)) from)
		   (< (fevt-from (cadr cur)) until))
	  (when (> (- (fevt-from (cadr cur)) (fevt-until (car cur)))
		   (/ limit 1000.0))
	    (message "%f: %f" (fevt-until (car cur))
		     (- (fevt-from (cadr cur)) (fevt-until (car cur))))
	    (push (cons (cdar cur) (caadr cur)) sched)))
	(setf cur (cdr cur))))
    (org-insert-ftrace-sched (nreverse sched))))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(provide 'ftrace)
