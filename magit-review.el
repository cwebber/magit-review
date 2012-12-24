
; This is a BUFFER LOCAL VARIABLE, do not set.
(defvar magit-review/review-state
  nil
  "State of reviews in this magit-review buffer.

Contains the user marks of what is and isn't in what state of review.

Doesn't contain info on whether or not there's anything new to be
seen in the branch.

Buffer-local; do not set manually!")

;;; Format of review metadata
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
