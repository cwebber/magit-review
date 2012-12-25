;; Magit-reviewer
;; --------------
;;
;; Copyright (C) 2012, Christopher Allan Webber
;;
;; magit-reviewer is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;; This is a BUFFER LOCAL VARIABLE, do not set.
(defvar magit-review/review-state
  nil
  "State of reviews in this magit-review buffer.

Contains the user marks of what is and isn't in what state of review.

Doesn't contain info on whether or not there's anything new to be
seen in the branch.

Buffer-local; do not set manually!")
(make-variable-buffer-local 'magit-review/review-state)

(defvar magit-review/review-state-changed
  nil
  "Whether or not the review state has changed since last being serialized")
(make-variable-buffer-local 'magit-review/review-state-changed)


;;; Format of review metadata
;;; -------------------------
;; 
;; Here's a mini json representation:
;; 
;;   {"refs/remotes/bretts/bug419-chunk-reads": {
;;      "state": "tracked:review",
;;      "notes": "We'll come back to this later"},
;;    "refs/remotes/bretts/keyboard_nav": {
;;      "state": "tracked:deferred"},
;;    "refs/remotes/bretts/master": {
;;      "state": "tracked:review"},
;;    "refs/remotes/bretts/newlayout": {
;;      "state": "ignored:ignored"},
;;    "refs/remotes/bretts/newlayout-stage": {
;;      "state": "ignored:nothingnew"},
;;    "refs/remotes/chemhacker/349_rssfeed": {
;;      "state": "ignored:ignored"},
;;    "refs/remotes/chemhacker/bug178_userlist": {
;;      "state": "ignored:nothingnew"}}


;; Review file management
;; ----------------------
;; 
;; Review file is jsonified versions of the local
;; magit-review/review-state alist variable.

; Get the review file
(defun magit-review/get-review-file ()
  (concat (magit-git-dir) "info/magit-review"))


; Load review file
(defun magit-review/read-review-file ()
  "Read the review file and get back an alist"
  (let ((magit-review-file magit-review/get-review-file)
        (json-key-type 'string)
        (json-object-type 'alist))
    (if (file-exists-p magit-review-file)
        (json-read-file magit-review-file))))

(defun magit-review/load-review-file ()
  "Read the review file into the buffer-local state of reviewing"
  (setq magit-review/review-state (magit-review/read-review-file)))


; Serialize current state
(defun magit-review/serialize-review-state ()
  "Serialize the current state of reviews to file."
  (let ((magit-review-file magit-review/get-review-file)
        (jsonified-review-state
         (json-encode-alist magit-review/review-state)))
    ; Move the old file aside
    (magit-review/move-old-review-file-if-exists)
    ; Write a new file
    (with-temp-file
        magit-review-file
      (insert jsonified-review-state))))
   

(defun magit-review/move-old-review-file-if-exists ()
  "If the old magit-review file exists, move it aside."
  (let ((magit-review-file magit-review/get-review-file))
    (if (file-exists-p magit-review-file)
        (rename-file magit-review-file
                     (concat (magit-git-dir) "info/magit-review.old")
                     t))))


;; Branch filtering
;; ----------------

(defvar magit-review/filter-rule
  "tracked=all ignored=none other=new"
  "String to state how things are being filtered.

Basically, with a string like:

  tracked=all ignored=none other=new

this will mean:
 - All tracked branches (eg, tracked:review or tracked:deferred,
   whatever) will be shown, regardless of whether there's new commits or not.
 - All ignored branches (eg, ignored:ignored and
   ignored:nothingnew) will be ignored, regardless of whatever
 - Anything else will be displayed, but *only* if there are new commits.

You can get more specific also, like:
 tracked:review=all tracked:deferred=new ignored=none other=new
")

(defun magit-review/parse-filter-string (filter-string)
  "Take a filter string and break it into filter coponents.

So:
  tracked=all ignored=none other=new

will become:
  ((\"tracked\" . 'all)
   (\"ignored\" . 'none)
   (\"other\" . 'new))"
  (mapcar
   (lambda (item) (split-string item "="))
   (split-string "tracked=all ignored=none other=new")))
