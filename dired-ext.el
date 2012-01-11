;;; dired-ext.el --- Extension functions for dired

;; Copyright (C) 2010  

;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Keywords: dired, utility

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

;;; Commentary:

;; 

;;; Code:
(eval-when-compile (require 'cl))
(defun* dired-find-duplicates (files dir)
  "Find duplicates of files and put them in a dired buffer.
FILES is a list of files which will be compared. DIR is the directory
which will be checked for duplicates of any of the files in the list.
Any matching files will be placed in a new dired buffer with name
*duplicated files*.
When called interactively from a dired buffer, the marked files in that dired buffer will
be treated as the orginals whose duplicates are to be found, and the user will be prompted
for a directory to search for duplicates.
If the function is called with 1 prefix arg then the original files that have duplicates
will be marked for deletion.
With 2 prefix args the files in the *duplicate files* buffer will be marked for deletion.
With 3 prefix args the original files will be placed in the *duplicated files* buffer,
interleaved with the duplicates (original file first, followed by duplicates), and 
the original files will be marked for deletion.
With 4 prefix args the behaviour is the same as with 3 prefix args except that the 
duplicate files will be marked for deletion instead of the originals."
  (interactive (list (dired-get-marked-files)
		     (read-directory-name "Directory to be checked: ")))

  (let ((original-buffer-name (buffer-name))
	(curr-arg (or (and (listp current-prefix-arg)
			   (car current-prefix-arg))
		      current-prefix-arg))
	(orignal-matched-files nil)
	(duplicated-matched-files nil))
    
    ;; kill buffer *duplicated files* if it exists
    (dired-ext-kill-buffer "*duplicated files*")
    
    (dired-do-shell-command "md5sum" nil files)
    (let ((marked-pair (dired-ext-md5-file-pair))
	  (tobe-checked-pair (progn
			       (shell-command (format "find %s -type f -exec md5sum {} \\;" dir))
			       (dired-ext-md5-file-pair))))
      ;; find the matched files
      (dolist (pair marked-pair)
	(mapc #'(lambda (arg)
		  (when (and (string-equal (car arg) (car pair))
			     ;; ignore the exactly same file
			     (not (string-equal (cdr arg) (cdr pair))))
		    (push (cdr pair) orignal-matched-files)
		    (push (cdr arg) duplicated-matched-files))) tobe-checked-pair))
      
      (dired-ext-kill-buffer "*Shell Command Output*"))

    (when (null duplicated-matched-files)
      ;; when there are no duplicated files, simply return to avoid the
      ;; error when calling (dired (cons "name" nil))
      (message "No duplicated files found!")
      (return-from dired-find-duplicates))
    
    (message "Find duplicated files done")
    
    (if (or (null curr-arg) (= curr-arg 4) (= curr-arg 16))
	(progn
	  (dired (cons "*duplicated files*" (reverse duplicated-matched-files)))
	  (switch-to-buffer original-buffer-name)
	  (when (and curr-arg (= curr-arg 4) files)	; C-u is used and there is marked file
	    (dired-map-over-marks
	     (let ((file-name (dired-get-filename)))
	       (when (member* file-name orignal-matched-files :test #'string-equal)
		 (dired-flag-file-deletion 1)))
	     nil))
	  (delete-other-windows)
	  (split-window-vertically)
	  (switch-to-buffer-other-window "*duplicated files*")
	  (when (and curr-arg (= curr-arg 16) duplicated-matched-files) ; C-u C-u is used
	    ;; all the files should be marked for deletion
	    (dired-map-dired-file-lines
	     #'(lambda (arg)
		 (dired-flag-file-deletion 1)))))
      
      ;; 3 or 4 prefix arg is used
      (let ((original-duplicate-list nil))
	(mapcar* #'(lambda (arg1 arg2)
		     (let ((find-it (member* arg1 original-duplicate-list :test #'string-equal :key #'car)))
		       (if find-it
			   (setf (car find-it) (append (car find-it) (list arg2)))
			 (push (list arg1 arg2) original-duplicate-list))))
		 orignal-matched-files
		 duplicated-matched-files)

	(dired (cons "*duplicated files*" (reduce #'append (reverse original-duplicate-list))))
	(switch-to-buffer "*duplicated files*")
	(delete-other-windows)
	
	(if (= curr-arg 64)
	    ;; mark orginal files for deletion
	    (dired-map-dired-file-lines
	     #'(lambda (file)
		 (when (member* file original-duplicate-list :test #'string-equal
				:key #'(lambda (arg)
					 (file-truename (car arg))))
		   (dired-flag-file-deletion 1))))
	    (if (= curr-arg 256)
		;; mark duplicated files for deletion
		(dired-map-dired-file-lines
		 #'(lambda (file)
		     (when (member* file duplicated-matched-files :test #'string-equal
				    :key #'(lambda (arg)
					     (file-truename arg)))
		       (dired-flag-file-deletion 1))))))))))

(defun dired-ext-md5-file-pair ()
  "Get an alist of (md5 . file) in buffer *Shell Command Output*."
  (with-current-buffer "*Shell Command Output*"
    (goto-char (point-min))
    (let ((lst nil))
      (while (not (eobp))
	(let* ((beg (point))
	       (end (re-search-forward " " (line-end-position) 't 1))
	       (md5 (buffer-substring beg end))
	       (file (progn
		       (skip-chars-forward "[ \t]")
		       (buffer-substring (point) (line-end-position)))))
	  (push (cons md5 file) lst)
	  (forward-line)))
      (nreverse lst))))

(defun dired-ext-kill-buffer (name)
  "When a buffer with name NAME exists, kill it."
  (when (get-buffer name)
    (kill-buffer name)))

(provide 'dired-ext)
;;; dired-ext.el ends here
