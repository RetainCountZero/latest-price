;;;; Program name: Latest-Price
;;;; Author:       Thomas Vossen
;;;; Copyright:    CrimsonMagic.net 2016

;;; Build instructions
;;; requires Clozure Common Lisp
;;; cd to the folder containing the source files
;;; path-to-your-lisp/dx86cl64 --load build.lisp

(in-package #:latest-price)

(defun main ()
  "The main function."
  ;; First argument is process binary, second is input file
  (if (eq (length (uiop:raw-command-line-arguments)) 2)
      ;; Convert file
      (progn
        (let ((arg1 (second (uiop:raw-command-line-arguments))))
          (write-file (generate-output-filename (check-infile arg1))
                      (remove-old-prices (read-source-file (check-infile arg1))))
          (move-to-folder (generate-output-filename (check-infile arg1)) "AEP-Out"))
          ;;; (move-to-done (generate-output-filename (check-infile arg1))))
        (uiop:quit 0))

      ;; Called with wrong number of argments
      ;; Write some help and quit.
      (progn
        #+windows (format t "~%The LATEST-PRICE.exe application requires one argument.")
        #-windows (format t "~%The LATEST-PRICE application requires one argument.~%")
        (format t "The argument is the full path to the input file.~%")
        (format t "The result filename is saved with the added suffix")
        (format t "_conv.~%~%")
        #+windows (format t "Example: LATEST-PRICE.exe d:\\tmp\\aepreise_13.10.2015_18.10.2015.csv~%~%")
        #-windows (format t "Example: LATEST-PRICE /tmp/aepreise_13.10.2015_18.10.2015.csv~%~%")
        (uiop:quit 1))))


;;; Check for existence of source file.
;;; If file does not exist, quit application and do nothing.
(defun check-infile (infile)
  (if (probe-file (pathname infile))
      (progn
        (unless (eql :ABSOLUTE (first (pathname-directory (pathname infile))))
          (progn
            (format t "Input file has to be supplied as absolute filename.~%")
            (format t "Example d:\\path-to-input\\filename.csv~%")
            (uiop:quit 2)))
        (unless (equal "csv" (pathname-type (pathname infile)))
          (progn
            (format t "Input file has to be a .csv named file. Exiting.~%")
            ;;; (move-to-junk-folder infile)
            (move-to-folder infile "AEP-Junk")
            (uiop:quit 3)))
        (pathname infile))
      (progn
        (format t "Input file does not exist. Exiting.~%")
        (uiop:quit 4))))

;;; The converted file is saved under a new name.
;;; Source file: abc.csv -> converted file: abc_conv.csv
(defun generate-output-filename (file &optional (suffix "_conv"))
  "Adds the SUFFIX to a FILE."
  (let ((filename (pathname-name (pathname file)))
        (extension (pathname-type (pathname file))))
    (make-pathname :name (concatenate 'string filename suffix)
                   :type extension
                   :defaults (pathname file))))

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

;;; Move a file to DONE folder
;;; (defun move-to-done (file &optional (done-folder "AEP-Out"))
;;;   "Move a file (supplied as absolute pathname) to DONE folder"
;;;   (let* ((oldfile (pathname file))
;;;          (newpath (append (butlast (pathname-directory (pathname file)))
;;;                           (list done-folder)))
;;;          (newfile (make-pathname :directory newpath
;;;                                  :defaults file)))
;;;     (rename-file oldfile (ensure-directories-exist newfile) :if-exists :overwrite)))

;;; Move a file to JUNK folder
;;; (defun move-to-junk (file &optional (junk-folder "AEP-Junk"))
;;;   "Move a file (supplied as absolute pathname) to JUNK folder"
;;;   (let* ((oldfile (pathname file))
;;;          (newpath (append (butlast (pathname-directory (pathname file)))
;;;                           (list junk-folder)))
;;;          (newfile (make-pathname :directory newpath
;;;                                  :defaults file)))
;;;     (rename-file oldfile (ensure-directories-exist newfile) :if-exists :overwrite)))

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
        (format t "File contains a line with != 11 columns. Exiting.~%")
        (move-to-junk (check-infile (second (uiop:raw-command-line-arguments))) "AEP-Junk")
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



