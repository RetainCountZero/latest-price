;;;; Program name: Latest-Price
;;;; Author:       Thomas Vossen
;;;; Copyright:    CrimsonMagic.net 2016

;;; Build instructions
;;; requires Clozure Common Lisp
;;; cd to the folder containing the source files
;;; path-to-your-lisp/dx86cl64 --load build.lisp

;;; When called from KiDSM with the arguments ${file} ${proc_outputDir}
;;; the implicit arguments for the main functions are like this:
;;;
;;; arg0: C:\Kisters\Konverter\latest-price.exe
;;; arg1: C:\Kisters\BelVisData\KiDSM\AEP-In\aepreise_13.10.2015_18.10.2015.csv
;;; arg2: C:\Kisters\KiDSM\kidsm\server\documents\processing\AEP-In\command.out

;;; ToDo: [Optional] handle the hidden (as of 5.161.0) argument ${DamagedDir}

(in-package #:latest-price)

(defun main ()
  "The main function."
  ;; First argument is process binary,
  ;; Second argument is input filename,
  ;; Third argument is the output path.
  (if (eq (length (uiop:raw-command-line-arguments)) 3)
      ;; Convert file
      (progn
        (let* ((arg1 (second (uiop:raw-command-line-arguments)))
               (arg2 (third (uiop:raw-command-line-arguments)))
               (in (check-infile arg1)))
           (write-file (kidsm-proc-pathname in (kidsm-proc-folder arg2))
                      (remove-old-prices (read-source-file in))))
           ;;; Move to AEP-Out is handled by KiDSM
        (uiop:quit 0))

      ;; Main was called with wrong number of argments.
      ;; Write some help and quit.
      (progn
        #+os-windows (format t "~%LATEST-PRICE.exe application requires two arguments.~%~%")
        #-os-windows (format t "~%LATEST-PRICE application requires two arguments.~%~%")
        (format t "The first argument is the full path to the input file,~%")
        (format t "second argument is the full path to the output folder.~%")
        (format t "The result filename is saved with the added suffix")
        (format t " _conv.~%~%")
        #+os-windows (format t "Example: LATEST-PRICE.exe d:\\tmp\\aepreise_20151013.csv d:\\tmp\\out~%~%")
        #-os-windows (format t "Example: LATEST-PRICE /tmp/aepreise_20151013.csv /tmp/out~%~%")
        (format *error-output* "Latest-Price: Called converter with no arguments.~%")
        (uiop:quit 1))))

;;; KiDSM supplies the folder without a trailing backslash
;;; This function always returns a folder PATHNAME.
(defun kidsm-proc-folder (arg)
  "Returns a PATHNAME from arg."
  (truename arg))

;;; KiDSM expects the converted file in this temporary location
;;; KiDSM transfers the converted file later to the Datenziel
(defun kidsm-proc-pathname (in-pathname out-pathname &optional (suffix "_conv"))
  "Returns a PATHNAME based on name of IN-PATHNAME and directory of OUT-PATHNAME."
  (let ((in-name (pathname-name in-pathname))
        (in-type (pathname-type in-pathname)))
    (make-pathname :name (concatenate 'string in-name suffix)
                   :type in-type
                   :defaults out-pathname)))

;;; Check for existence and possible problems of source file.
;;; If a problem is detected, an error is written to error-output.
;;; The application quits with an error code.
;;;
;;; 1 - Latest-Price: Called converter with no arguments
;;; 2 - Latest-Price: File ~a does not exist.
;;; 3 - Latest-Price: File ~a has to be supplied as absolute filename.
;;; 4 - Latest-Price: File ~a has to be of filetype CSV.
;;; 5 - Latest-Price: Line does not contain 11 columns.
;;;
(defun check-infile (infile)
  (let ((in (pathname infile)))
    (cond ((eql (probe-file in) nil)
           (progn
             (format *error-output*
                     "Latest-Price: File ~a does not exist.~%" in)
             (uiop:quit 2)))
          ((or (eql :RELATIVE (first (pathname-directory in)))
               (eql (first (pathname-directory in)) nil))
           (progn
             (format *error-output*
                     "Latest-Price: File ~a has to be supplied as absolute filename.~%" in)
             (move-to-folder in "AEP-Junk")
             (uiop:quit 3)))
          ((not (equal "csv" (pathname-type in)))
           (progn
             (format *error-output*
                     "Latest-Price: File ~a has to be of filetype CSV.~%" in)
             (move-to-folder in "AEP-Junk")
             (uiop:quit 4)))
          (t in))))

;;; Read the source file and return the list of strings as result
;;; The input file is expected to be a csv file.
(defun read-source-file (file)
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist nil)
    ;; Don't do anything if no file exists ...
    (unless (eql in nil)
      (cl-csv:read-csv in
                       :separator #\;
                       :skip-first-p t
                       :map-fn #'trim-and-encode))))

;;; Convert universal time of first and second column to human time
(defun csv-out (line)
  "Takes an input line with universal-time"
  (format nil "~a;~a;~a;~a;~a"
          (decoded-date (first line))
          (decoded-datetime (second line))
          (third line)
          (fourth line)
          (fifth line)))

;;; Write everything to file
(defun write-file (file content)
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line "Gastag;Stand;VHP_IND;AE_NEG;AE_POS" out)
    (loop for line in content do
      (write-line (csv-out line) out))))

;;; Move a FILE to a FOLDER
(defun move-to-folder (file folder)
  "Move a FILE (supplied as absolute pathname) to FOLDER"
  (let* ((oldfile (pathname file))
         (newpath (append (butlast (pathname-directory (pathname file)))
                          (list folder)))
         (newfile (make-pathname :directory newpath
                                 :defaults file)))
    (rename-file oldfile (ensure-directories-exist newfile) :if-exists :overwrite)))

(defun trim-and-encode (input)
  "Takes a list of 11 values and returns a list comprised of
elements 1-3 and 10-11."
  (if (eql (length input) 11)
      (list (encoded-date (first input))
            (encoded-datetime (second input))
            (third input)
            (elt input 9)
            (elt input 10))
      (progn
        (format *error-output*
                "Latest-Price: File contains a line ~a with != 11 columns.~%" input)
        (move-to-folder (check-infile (second (uiop:raw-command-line-arguments))) "AEP-Junk")
        (uiop:quit 5))))

(defun encoded-date (string)
  "Expects a string like 'DD.MM.YYYY'"
  (multiple-value-bind (date month year)
      (values
       (parse-integer (subseq string 0 2))
       (parse-integer (subseq string 3 5))
       (parse-integer (subseq string 6 10)))
    (encode-universal-time 00 00 00 date month year)))

(defun encoded-datetime (string)
  "Expects a string like 'DD.MM.YYYY hh24:mi'"
  (multiple-value-bind (date month year hour minute)
      (values
       (parse-integer (subseq string 0 2))
       (parse-integer (subseq string 3 5))
       (parse-integer (subseq string 6 10))
       (parse-integer (subseq string 11 13))
       (parse-integer (subseq string 14 16)))
    (encode-universal-time 00 minute hour date month year)))

(defun decoded-date (date)
  "Returns DATET as a string of the format 'DD.MM.YYYY'"
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time date)
    (declare (ignore second))
    (declare (ignore minute))
    (declare (ignore hour))
    (format nil "~2,'0d.~2,'0d.~2,'0d" day month year)))

(defun decoded-datetime (datetime)
  "Return DATETIME as a string of the format 'DD.MM.YYYY hh24:mi'"
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time datetime)
    (declare (ignore second))
    (format nil "~2,'0d.~2,'0d.~4,'0d ~2,'0d:~2,'0d" day month year hour minute)))

(defun get-gastage (list)
  "Returns a list of all first column (Gastag) values."
  (mapcar #'first list))

(defun gastag-exists (entry list)
  "Returns the ENTRY if ENTRY is contained in LIST."
  (when (member (first entry) (get-gastage list))
    entry))

(defun newer-entry-p (entry list-entry)
  "Returns T if entry is newer than list-entry."
  (when (> (second entry) (second list-entry))
    t))

(defun remove-old-prices (input)
  "INPUT is parsed. Keep only the most recent prices. Discard every other prices."
  (let ((result nil))
    (mapcar #'(lambda (entry)
                (if (gastag-exists entry result)
                    ;; replace existing entry if newer entry is supplied
                    (progn
                      (let ((idx (position (first entry) (get-gastage result))))
                        (when (newer-entry-p entry (elt result idx))
                          (progn
                            (setf result (remove (elt result idx) result :test #'equal))
                            (push entry result)))))
                    ;; add new entry
                    (push entry result)))
            input)
    ;; #'> => newest date first
    (sort result #'> :key #'first)))



