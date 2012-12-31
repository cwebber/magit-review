;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Magit-review
;; ------------
;;
;; Copyright (C) 2012, Christopher Allan Webber
;;
;; magit-review is free software; you can redistribute it and/or
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

;; This borrows significant ideas (and a few small code snippets) from
;; magit wazzup.


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

(defvar magit-review-head nil
  "The integration head for the current review buffer.
This is only non-nil in review buffers.")
(make-variable-buffer-local 'magit-review-head)


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

Valid states are... well anything, though convention is to use
\"tracked\" and \"ignored\" for keeping note of what to track or
ignore... but you could use anything.  There are some special
cases though... \"unknown\" means it does not have any state
assigned to it, and \"other\" is used to say \"anything that has
not matched yet will match this\", and you should always set it
last in your filter rule if using.

Valid directives are:
 - all (show everything)
 - none (show nothing)
 - new (show only things with new commits)
 - nothing-new (show only things that have no new commits)
")

(defun magit-review/parse-filter-string (&optional filter-string)
  "Take a filter string and break it into filter coponents.

So:
  tracked=all ignored=none other=new

will become:
  ((\"tracked\" \"all\")
   (\"ignored\" \"none\")
   (\"other\" \"new\"))

You can pass this FILTER-STRING; otherwise it will process
magit-review/filter-rule"
  (let ((filter-string (or filter-string magit-review/filter-rule)))
    (mapcar
     (lambda (item) (split-string item "="))
     (split-string filter-string))))


(defun magit-review/determine-matching-rule (branch-state rules)
  "Return a matching rule... if we can find one.

Note: if the branch doesn't have a state, it's convention to set
it as \"untracked\" before passing it in here.
"
  (let ((branch-state-components (split-string branch-state ":")))
    (block matching-rule-finder
      (loop
       for rule in rules do
       (let* ((rule-state (car rule))
              (rule-state-components (split-string rule-state ":")))
         (if (or
              ; it's other, which should always go last, so this is a catch-all
              (equal rule-state "other")
              ; they're the same thing; that's a match
              (equal rule-state branch-state)
              ; this is a "catch-many" rule, and it's multi-component
              (and (= (length rule-state-components) 1)
                   (> (length branch-state-components) 1)
                   (equal (car branch-state-components) rule-state))
              ; the branch state is none and this is an "untracked" rule
              (and (equal branch-state nil)
                   (equal rule-state "untracked")))
             (return-from matching-rule-finder rule)))))))


(defun magit-review/has-new-commits (branch-name &optional head)
  "See if this branch has any new commits in it."
  (> (length (magit-git-lines
              "log" "--pretty=oneline"
              (concat (or head "HEAD") ".." branch-name)))
     0))

(defun magit-review/should-include-branch (branch-name rule-directive &optional head)
  "Should we include anything new in this branch? Check!"
  (cond
   ; always include branches under an "all" directive
   ((equal rule-directive "all") t)
   ; never include any branches marked none
   ((equal rule-directive "none") nil)
   ((and (equal rule-directive "new")
         (magit-review/has-new-commits branch-name head)) t)
   ((and (equal rule-directive "nothing-new")
         (not (magit-review/has-new-commits branch-name head))) t)))


(defvar magit-review/default-directive
  "new"
  "Default directive if we don't find a matching rule.")


(defun magit-review/filter-branches (&optional head refs-to-check filter-rules)
  "Return a filtered set of branches

This function weeds out the ones that shouldn't be shown.

The returned a plist which will look something like:
  ((\"untracked\" . (\"refs/remotes/bretts/keyboard_nav\"
                     \"refs/remotes/bretts/master\")
   (\"tracked:review\" . (\"refs/remotes/bretts/newlayout\"
                          \"refs/remotes/bretts/newlayout-stage\"))))
"
  (let ((refs-to-check
         (or refs-to-check
             (magit-list-interesting-refs)))
        (filter-rules (or filter-rules (magit-review/parse-filter-string)))
        (filtered-branches (make-hash-table :test 'equal)))
    (mapc
     (lambda (branch)
       (let* ((branch-name (car branch))
              (branch-ref (cdr branch))
              (branch-state
               (or (assoc "state"
                          (assoc branch-ref
                                 magit-review/review-state))
                   "unknown"))
              ;; (branch-rule
              ;;  (magit-review/determine-matching-rule
              ;;   branch-ref branch-state filter-rules))
              (matching-rule
               (magit-review/determine-matching-rule
                branch-state filter-rules))
              (matching-rule-state
               (if matching-rule
                   (car matching-rule)
                 "unknown"))
              (matching-rule-directive
               (if matching-rule
                   (nth 1 matching-rule)
                 magit-review/default-directive)))
         ; If we should include the branch, let's include it!
         (if (magit-review/should-include-branch
              branch-ref matching-rule-directive head)

             ; File this branch with the other branches of its type
             (puthash
              branch-state
              (cons branch (gethash branch-state filtered-branches))
              filtered-branches))))
     refs-to-check)
    filtered-branches))

;; magit-review display
(defun magit-review/refresh-review-buffer (head)
  ; All doesn't make sense here.

  (setq magit-review-head head)
  (magit-create-buffer-sections
    (let ((branches-to-show (magit-review/filter-branches head)))
      (maphash
       (lambda (state branches)
         (magit-with-section 'reviewbuf nil
           (insert (format "Branches in %s:\n\n" state))
           (dolist (branch branches)
             (let* ((branch-name (car branch))
                    (branch-ref (cdr branch))
                    (magit-section-hidden-default t)
                    (n (length (magit-git-lines "log" "--pretty=oneline"
                                                (concat head ".." branch-ref)))))
               (if (> n 0)
                   (magit-set-section-info
                    branch-ref
                    (magit-git-section
                     (cons branch-ref 'review)
                     (format "%s unmerged commits in %s"
                             n branch-name)
                     'magit-wash-log
                     "log"
                     (format "--max-count=%s" magit-log-cutoff-length)
                     "--abbrev-commit"
                     (format "--abbrev=%s" magit-sha1-abbrev-length)
                     "--graph"
                     "--pretty=oneline"
                     (format "%s..%s" head branch-ref)
                     "--"))
                 (progn
                   ; I'm not sure whether or not this needs to be a section
                   (magit-with-section branch-ref 'review
                     (insert
                      (propertize
                       (format "(no commits) %s" branch-name)
                       'face 'magit-section-title)
                      "\n"))
                   (insert "\n")))))))
       branches-to-show))))


;; Keys stuff
;; ----------

(defvar magit-review/filter-bookmarks
  '(("g" "General" "tracked=all ignored=none other=new")
    ("tr" "Tracked review" "tracked:review=new other=none")
    ("ia" "Ignored all" "ignored=all other=none")
    ("ii" "ignored:ignored all" "ignored:ignored=all other=none")
    ("in" "ignored new" "ignored=new other=none")
    ("a" "All" "other=all"))
  "Note that after running this you probably want to eval
  (magit-review/add-filter-bookmark-keys)")


(defun magit-review/apply-filter (filter)
  (make-local-variable 'magit-review/filter-rule)
  (setq magit-review/filter-rule filter))

(defun magit-review/apply-filter-and-refresh (filter)
  (interactive)
  (magit-review/apply-filter filter)
  (magit-review/add-filter-bookmark-keys)
  (magit-review/refresh-review-buffer
   (or magit-review-head "HEAD")))


(defun magit-review/add-filter-bookmark-keys ()
  "Add filter/bookmark keys"
  ;; (re-)create the group
  (magit-key-mode-add-group 'review-bookmark)
  (loop
   for (key description filter) in magit-review/filter-bookmarks do
   (progn
     (magit-key-mode-insert-action
      'review-bookmark key description
      ; Generate a curried function that changes the filter
      (let ((this-filter filter))
        (lambda ()
          (interactive)
          (magit-review/apply-filter-and-refresh this-filter))))))
  (magit-key-mode-generate 'review-bookmark))


(defvar magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'magit-key-mode-popup-review-bookmark)
    map))

(magit-review/add-filter-bookmark-keys)


;; Mode stuff
;; ----------


(define-derived-mode magit-review-mode magit-mode "Magit Review"
  "Mode for looking at commits that could be merged from other branches.

\\{magit-review-mode-map}"
  :group 'magit)

(defun magit-review ()
  (interactive)
  (let ((topdir (magit-get-top-dir default-directory))
        (current-branch (magit-get-current-branch)))
    (magit-buffer-switch "*magit-review*")
    (magit-mode-init topdir 'magit-review-mode
                     #'magit-review/refresh-review-buffer
                     current-branch)))
