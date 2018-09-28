;;; Test for `resize-window'

;;; Commentary:
;; These are the tests for `resize-window'

;;; Code:

(defmacro aliases=> (alias original)
  `(should (equal ,original (resize-window--match-alias ,alias))))

(ert-deftest should-match-aliases ()
  (aliases=> 'right ?f)
  (aliases=> 'left ?b)
  (aliases=> 'up ?p)
  (aliases=> 'down ?n))

(ert-deftest should-return-original-if-no-alias ()
  (aliases=> ?d ?d))

(defvar choice-no-capital '(?n 'function "documentation" nil))
(defvar choice-capital '(?n 'function "documentation" t))

(ert-deftest should-create-documentation-from-alist ()
  (should (equal " n  : documentation"
                 (resize-window--display-choice choice-no-capital)))
  (should (equal "n|N : documentation"
                 (resize-window--display-choice choice-capital))))

(ert-deftest should-execute-and-display-message ()
  (let ((choice '(?n (lambda () (setq executed t)) "doc" nil))
        resize-window-notify-with-messages             ;suppress messages
        (executed))
    (resize-window--execute-action choice)
    (should executed))

  (let ((bad-arity '(?n (lambda (a b c) 1) "doc" nil))
        resize-window-notify-with-messages)
    ;; this is a bad arity test. it should not error.
    (resize-window--execute-action bad-arity)))

(ert-deftest should-identify-which-allow-capital-matching ()
  (should (resize-window--allows-capitals choice-capital))
  (should-not (resize-window--allows-capitals choice-no-capital)))

;;; tests for swapping uppercase and lowercase behavior
(ert-deftest resize-window-swap-capital-and-lowercase-behavior-swaps ()
  (let ((resize-window-swap-capital-and-lowercase-behavior t))
    (should (equal resize-window-coarse-argument
                   (resize-window-lowercase-argument)))
    (should (equal resize-window-fine-argument
                   (resize-window-uppercase-argument)))))

(ert-deftest resize-window-swap-capital-and-lowercase-behavior-ignored ()
  (let ((resize-window-swap-capital-and-lowercase-behavior))
    (should (equal resize-window-coarse-argument
                   (resize-window-uppercase-argument)))
    (should (equal resize-window-fine-argument
                   (resize-window-lowercase-argument)))))

(ert-deftest resize-window--key-already-used-tests ()
  (should (resize-window--key-available? ?v))
  (should-not (resize-window--key-available? ?n)))

;;; tests to dissect the stack utility functions
(defun assert-deep-copy* (a b)
  "Return non-nil if A and B are unshared duplicates.
Return nil otherwise.

Unshared duplicates are `equal' values which are not sequences or
empty sequences, or sequences which are `equal' but not `eq'.  In
case of sequences, all of their members adhere to this criterion,
i.e. two list of lists certainly are unshared duplicates when one
is the `copy-tree' the other (deep copy).

Shared duplicates are `eq' sequences or `equal' sequences holding
at least one `eq' member (shared)."
  (and (equal a b)
       (or (not a)
           (not (seqp a))
           (not (eq a b)))
       (if (and (seqp a) (not (seq-empty-p a)))
           ;; properly handle improper lists
           (if (listp a)
               (and (assert-deep-copy* (car a) (car b))
                    (assert-deep-copy* (cdr a) (cdr b)))
             (let ((res t)
                   (len (seq-length a)))
               (dotimes (n len res)
                 (setq res
                       (and res
                            (assert-deep-copy*
                             (seq-elt a n) (seq-elt b n))))
                 (unless res
                   (setq n len)))))
         t)))

(defun find-shared-member (&rest containers)
  "Find a stack member shared amongst all CONTAINERS.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (let ((list-of-members (car containers))
        (rest-containers (cdr containers)))
    (unless (consp (car list-of-members))
      (setq list-of-members (cdr list-of-members)))
    (catch 'done
      (dolist (stack-member list-of-members)
        (catch 'next-stack-member
          (let (match)
            (dolist (container rest-containers)
              (unless (setq match (memq stack-member container))
                (throw 'next-stack-member nil)))
            (when match
              (throw 'done stack-member))))))))

(defun find-shared-member* (&rest containers)
  "Find a stack member shared by at least two CONTAINERS.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (let ((list-of-members (car containers))
        (rest-containers (cdr containers)))
    (when rest-containers
      (unless (consp (car list-of-members))
        (setq list-of-members (cdr list-of-members)))
      (catch 'done
        (dolist (stack-member list-of-members)
          (dolist (container rest-containers)
            (when (memq stack-member container)
              (throw 'done stack-member))))
        (apply #'find-shared-member* (cdr containers))))))

(defun resize-window--dummy-window-stacks ()
  "Return a dummy version of `resize-window--window-stacks'.

Return a deep copy of the dummy list of frame stacks. There is no
risk to permanently modifying the return value. It's granted that
each returned copy is equal to the first one."
  (copy-tree '((frame-a . ((a . 0) (b . 1) (c . 2)))
               (frame-b . ((d . 3) (e . 4) (f . 5)))
               (frame-c . ((g . 6) (h . 7) (i . 8)))
               (frame-x . ()))))

(defmacro resize-window--bind-dummy-stacks (&rest body)
  "Bind a copy of the dummy list of frame stacks then eval BODY.
The value of the last form in BODY is returned.

Bind a `resize-window--dummy-window-stacks' return value to the
symbols described below (see the former symbol):

List of frame stacks ((frame-sym . ((cfg-sym . time-num)...))):
`resize-window--window-stacks' (see the former symbol)

List of frame stacks backup, unshared duplicate:
`window-stacks-bck' (deep copy of `resize-window--window-stacks')

Frame stack (frame-sym . ((cfg-sym . time-num)...)):
`frame-stack-a' (1st one of `resize-window--window-stacks')
`frame-stack-b' (2nd one of `resize-window--window-stacks')
`frame-stack-c' (3rd one of `resize-window--window-stacks')
`frame-stack-x' (4th one of `resize-window--window-stacks')
`frame-stack-y' (null frame stack, reserved to fake frames)
`frame-stack-z' (null frame stack, reserved to true frames)

Frame stack alternate, starts as unshared duplicate:
`frame-stack-a-alt' (deep copy of `frame-stack-a')
`frame-stack-b-alt' (deep copy of `frame-stack-b')
`frame-stack-c-alt' (deep copy of `frame-stack-c')
`frame-stack-x-alt' (deep copy of `frame-stack-x')
`frame-stack-y-alt' (deep copy of `frame-stack-y')
`frame-stack-z-alt' (deep copy of `frame-stack-z')

frame-sym, symbol representing a frame:
`frame-a' (frame of `frame-stack-a')
`frame-b' (frame of `frame-stack-b')
`frame-c' (frame of `frame-stack-c')
`frame-x' (frame of `frame-stack-x')
`frame-y' (new/unsaved fake frame)
`frame-z' (reserved to true frame)

Stack, list of stack members ((cfg-sym . time-num)...):
`stack-a' (stack associated with `frame-a')
`stack-b' (stack associated with `frame-b')
`stack-c' (stack associated with `frame-c')
`stack-x' (stack associated with `frame-x')
NOTE: `stack-x' is empty.

Stack member (cfg-sym . time-num):
`member-a0' `member-a1' `member-a2' (stack members of `stack-a')
`member-b0' `member-b1' `member-b2' (stack members of `stack-b')
`member-c0' `member-c1' `member-c2' (stack members of `stack-c')
NOTE: `stack-x' is empty, hence it doesn't have stack members.

cfg-sym, symbol representing a window configuration:
`config-a0' `config-a1' `config-a2' (configurations of `stack-a')
`config-b0' `config-b1' `config-b2' (configurations of `stack-b')
`config-c0' `config-c1' `config-c2' (configurations of `stack-c')
NOTE: `stack-x' is empty, hence it doesn't have configurations.

time-num, number representing a configuration's saved time:
`time-a0' `time-a1' `time-a2' (saved times of `stack-a')
`time-b0' `time-b1' `time-b2' (saved times of `stack-b')
`time-c0' `time-c1' `time-c2' (saved times of `stack-c')
NOTE: `stack-x' is empty, hence it doesn't have saved times."
  (declare (indent 0) (debug (body)))
  `(let* ((resize-window--window-stacks
           (resize-window--dummy-window-stacks))
          (window-stacks-bck (copy-tree resize-window--window-stacks))
          (frame-stack-a (nth 0 resize-window--window-stacks))
          (frame-stack-b (nth 1 resize-window--window-stacks))
          (frame-stack-c (nth 2 resize-window--window-stacks))
          (frame-stack-x (nth 3 resize-window--window-stacks))
          (frame-stack-y nil)
          (frame-stack-z nil)
          (frame-stack-a-alt (copy-tree frame-stack-a))
          (frame-stack-b-alt (copy-tree frame-stack-b))
          (frame-stack-c-alt (copy-tree frame-stack-c))
          (frame-stack-x-alt (copy-tree frame-stack-x))
          (frame-stack-y-alt (copy-tree frame-stack-y))
          (frame-stack-z-alt (copy-tree frame-stack-z))
          (frame-a (car frame-stack-a))
          (frame-b (car frame-stack-b))
          (frame-c (car frame-stack-c))
          (frame-x (car frame-stack-x))
          (frame-y 'frame-y)
          (frame-z nil)
          (stack-a (cdr frame-stack-a))
          (stack-b (cdr frame-stack-b))
          (stack-c (cdr frame-stack-c))
          (stack-x (cdr frame-stack-x))
          (member-a0 (nth 0 stack-a))
          (member-a1 (nth 1 stack-a))
          (member-a2 (nth 2 stack-a))
          (config-a0 (car member-a0))
          (config-a1 (car member-a1))
          (config-a2 (car member-a2))
          (time-a0 (cdr member-a0))
          (time-a1 (cdr member-a1))
          (time-a2 (cdr member-a2))
          (member-b0 (nth 0 stack-b))
          (member-b1 (nth 1 stack-b))
          (member-b2 (nth 2 stack-b))
          (config-b0 (car member-b0))
          (config-b1 (car member-b1))
          (config-b2 (car member-b2))
          (time-b0 (cdr member-b0))
          (time-b1 (cdr member-b1))
          (time-b2 (cdr member-b2))
          (member-c0 (nth 0 stack-c))
          (member-c1 (nth 1 stack-c))
          (member-c2 (nth 2 stack-c))
          (config-c0 (car member-c0))
          (config-c1 (car member-c1))
          (config-c2 (car member-c2))
          (time-c0 (cdr member-c0))
          (time-c1 (cdr member-c1))
          (time-c2 (cdr member-c2)))
     ,@body))

(defmacro backup-window-stacks ()
  "Backup a deep copy of the list of frame stacks.
Deep copy `resize-window--window-stacks' to `window-stacks-bck'.
See bindings in `resize-window--bind-dummy-stacks'."
  `(setq window-stacks-bck (copy-tree resize-window--window-stacks)))

(defmacro assert-window-stacks-is-backup ()
  "Assert the list of frame stacks equals its latest backup.
Compare `resize-window--window-stacks' to `window-stacks-bck'.
See bindings in `resize-window--bind-dummy-stacks'."
  `(should (equal resize-window--window-stacks window-stacks-bck)))

(defmacro build-window-stacks (&rest frame-stacks)
  "Return the list of frame stacks built with FRAME-STACKS.
FRAME-STACKS may be nil to express an empty list of frame stacks.
Update `resize-window--window-stacks' to the value of the result.
See bindings in `resize-window--bind-dummy-stacks'."
  (declare (indent 0))
  (cond ((equal frame-stacks '(nil)) (setq frame-stacks nil))
        (frame-stacks (setq frame-stacks `(list ,@frame-stacks))))
  `(setq resize-window--window-stacks ,frame-stacks))

(defmacro build-frame-stack-with-members (frame &rest stack-members)
  "Return a frame stack associating FRAME with STACK-MEMBERS.
STACK-MEMBERS may be nil to express an empty stack."
  (declare (indent 0) (indent 1))
  `(cons ,frame (build-stack ,@stack-members)))

(defmacro build-frame-stack (frame &optional stack)
  "Return a frame stack associating FRAME with STACK.
STACK may be nil to express an empty stack."
  (declare (indent 0) (indent 1))
  `(cons ,frame ,stack))

(defmacro build-stack (&rest stack-members)
  "Return a stack built with STACK-MEMBERS.
If STACK-MEMBERS is nil return nil to express an empty stack."
  (declare (indent 0))
  (cond ((equal stack-members '(nil)) (setq stack-members nil))
        (stack-members (setq stack-members `(list ,@stack-members))))
  `,stack-members)

(defmacro build-member (config save-time)
  "Return a stack member built with CONFIG and SAVE-TIME."
  (declare (indent 0))
  `(cons ,config ,save-time))

(defmacro assert-result (expression result)
  "Assert that EXPRESSION is `eq' to RESULT."
  (declare (indent 0) (indent 1))
  `(should (eq ,expression ,result)))

(defmacro assert-deep-copy (expression-a expression-b)
  "Assert EXPRESSION-A and EXPRESSION-B are unshared duplicates.
See also `assert-deep-copy*'."
  (declare (indent 0))
  `(should (assert-deep-copy* ,expression-a ,expression-b)))

(defmacro assert-equal (expression-a expression-b)
  "Assert `equal' EXPRESSION-A and EXPRESSION-B."
  (declare (indent 0))
  `(should (equal ,expression-a ,expression-b)))

(defmacro assert-equal-frame-stacks (frame-stack-x frame-stack-y)
  "Assert `equal' FRAME-STACK-X and FRAME-STACK-Y.

The existence of this macro is as a remainder to test for frame
stacks equality. Could be easily replaced by `assert-equal'."
  (declare (indent 0))
  `(assert-equal ,frame-stack-x ,frame-stack-y))

(defmacro assert-equal-stacks (stack-x stack-y)
  "Assert `equal' STACK-X and STACK-Y.

The existence of this macro is as a remainder to test for stacks
equality. Could be easily replaced by `assert-equal'."
  (declare (indent 0))
  `(assert-equal ,stack-x ,stack-y))

(defmacro assert-equal-members (stack-member-x stack-member-y)
  "Assert `equal' STACK-MEMBER-X and STACK-MEMBER-Y.

The existence of this macro is as a remainder to test for stack
members equality. Could be easily replaced by `assert-equal'."
  (declare (indent 0))
  `(assert-equal ,stack-member-x ,stack-member-y))

(defmacro assert-framep (frame-stack)
  "Assert that FRAME-STACK has memorized a true frame."
  (declare (indent 0))
  `(should (framep (car ,frame-stack))))

(defmacro assert-frame-live-p (frame-stack)
  "Assert that FRAME-STACK has memorized a live frame."
  (declare (indent 0))
  `(should (frame-live-p (car ,frame-stack))))

(defmacro assert-selected-frame (frame-stack)
  "Assert that FRAME-STACK has memorized the selected frame."
  (declare (indent 0))
  `(should (eq (car ,frame-stack) (selected-frame))))

(defmacro assert-window-stacks (&rest frame-stacks)
  "Assert `equal' list of FRAME-STACKS and list of frame stacks.
FRAME-STACKS may be nil to express an empty list of frame stacks.
Evaluates `resize-window--window-stacks' as list of frame stacks.
See bindings in `resize-window--bind-dummy-stacks'."
  (declare (indent 0))
  (cond ((equal frame-stacks '(nil)) (setq frame-stacks nil))
        (frame-stacks (setq frame-stacks `(list ,@frame-stacks))))
  `(assert-equal ,frame-stacks resize-window--window-stacks))

(defmacro assert-member-shared-by-all (stack-member &rest containers)
  "Assert STACK-MEMBER is shared amongst all CONTAINERS.
There is a member in each CONTAINERS `eq' to STACK-MEMBER.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0) (indent 1))
  (let (body)
    (dolist (c containers)
      (push `(should (memq ,stack-member ,c)) body))
    (setq body (nreverse body))
    (cons 'progn body)))

(defmacro assert-member-unshared-by-all (stack-member &rest containers)
  "Assert STACK-MEMBER isn't shared amongst all CONTAINERS.
There is no member in CONTAINERS `eq' to STACK-MEMBER.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0) (indent 1))
  (let (body)
    (dolist (c containers)
      (push `(should-not (memq ,stack-member ,c)) body))
    (setq body (nreverse body))
    (cons 'progn body)))

(defmacro assert-member-shared-by-any (stack-member &rest containers)
  "Assert STACK-MEMBER is shared at least in one CONTAINERS.
There is at least a member in CONTAINERS `eq' to STACK-MEMBER.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0) (indent 1))
  (let (body)
    (dolist (c containers)
      (push `(memq ,stack-member ,c) body))
    (setq body (nreverse body))
    (push 'or body)
    (cons 'should (list body))))

(defmacro assert-member-unshared-by-any (stack-member &rest containers)
  "Assert STACK-MEMBER isn't shared in at least one CONTAINERS.
There is at least a CONTAINERS with no member `eq' to STACK-MEMBER.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0) (indent 1))
  (let (body)
    (dolist (c containers)
      (push `(memq ,stack-member ,c) body))
    (setq body (nreverse body))
    (push 'and body)
    (cons 'should-not (list body))))

(defmacro assert-all-share-a-member (&rest containers)
  "Assert there is a member shared amongst all CONTAINERS.
There is a member `eq' to one member in each CONTAINERS.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0))
  `(should (find-shared-member ,@containers)))

(defmacro assert-all-share-no-member (&rest containers)
  "Assert there's no member shared amongst all CONTAINERS.
There's no member `eq' to one member in each CONTAINERS.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0))
  `(should-not (find-shared-member ,@containers)))

(defmacro assert-any-share-a-member (&rest containers)
  "Assert there is a member shared between CONTAINERS.
A member in one CONTAINERS is `eq' to a member in another.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0))
  `(should (find-shared-member* ,@containers)))

(defmacro assert-any-share-no-member (&rest containers)
  "Assert there's no member shared between CONTAINERS.
No member in CONTAINERS is `eq' to a member in other ones.
CONTAINERS may be a mixed list of frame stacks and stacks."
  (declare (indent 0))
  `(should-not (find-shared-member* ,@containers)))

(defmacro assert-frame-stack-members (frame-stack &rest stack-members)
  "Assert that FRAME-STACK can be built with STACK-MEMBERS.
STACK-MEMBERS may be nil to express an empty stack."
  (declare (indent 0) (indent 1))
  `(assert-equal-frame-stacks
     ,frame-stack
     (build-frame-stack-with-members
         (car ,frame-stack)
       ,@stack-members)))

(defmacro assert-frame-stack (frame-stack &optional stack)
  "Assert that FRAME-STACK can be built with STACK.
STACK may be nil to express an empty stack."
  (declare (indent 0) (indent 1))
  `(assert-equal-frame-stacks
     ,frame-stack
     (build-frame-stack
         (car ,frame-stack)
       ,stack)))

(defmacro assert-stack (stack &rest stack-members)
  "Assert that STACK can be built with STACK-MEMBERS.
STACK-MEMBERS may be nil to express an empty stack."
  (declare (indent 0) (indent 1))
  `(assert-equal-stacks
     ,stack
     (build-stack ,@stack-members)))

(defmacro assert-member (stack-member config save-time)
  "Assert that STACK-MEMBER can be built with CONFIG and SAVE-TIME."
  (declare (indent 0) (indent 1))
  `(assert-equal-members
     ,stack-member
     (build-member ,config ,save-time)))

(defmacro assert-member-config (stack-member config)
  "Assert that STACK-MEMBER can be built using CONFIG."
  (declare (indent 0) (indent 1))
  `(assert-member
       ,stack-member
     ,config (cdr ,stack-member)))

(defmacro assert-member-svtime (stack-member save-time)
  "Assert that STACK-MEMBER can be built using SAVE-TIME."
  (declare (indent 0) (indent 1))
  `(assert-member
       ,stack-member
     (car ,stack-member) ,save-time))

(defmacro assert-window-stacks-is-dummy ()
  "Assert `resize-window--window-stacks' is a dummy.

See bindings in `resize-window--bind-dummy-stacks'.
See the dummy version `resize-window--dummy-window-stacks'."
  `(assert-deep-copy resize-window--window-stacks
                     (resize-window--dummy-window-stacks)))

(defmacro assert-dummy-window-stacks ()
  "Assert the dummy list of frame stacks is ready.
Test and show off stack macros usage.

See bindings in `resize-window--bind-dummy-stacks'.
See the dummy version `resize-window--dummy-window-stacks'."
  `(progn
     ;; test selected frame
     (setq frame-stack-z
           (build-frame-stack
               (setq frame-z (selected-frame))
             (setq stack-z
                   (build-stack member-a0))))
     (setq frame-stack-z-alt (copy-tree frame-stack-z))
     (assert-framep frame-stack-z-alt)
     (assert-frame-live-p frame-stack-z-alt)
     (assert-selected-frame frame-stack-z-alt)
     ;; test shared stack members
     (assert-member-shared-by-all
         member-a0
       stack-a
       stack-z
       frame-stack-a
       frame-stack-z
       (build-stack
         member-b0 member-a0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0))
     (assert-member-shared-by-any
         member-a0
       stack-a)
     (assert-member-shared-by-any
         member-a0
       frame-stack-a)
     (assert-member-shared-by-any
         member-a0
       (build-stack
         member-b0 member-a0 member-c0))
     (assert-member-shared-by-any
         member-a0
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0)))
     (assert-member-shared-by-any
         member-a0
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0))
     (assert-member-shared-by-any
         member-a0
       stack-x
       frame-stack-x
       frame-stack-b
       frame-stack-a-alt
       (build-stack
         member-b0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-c0)
       (build-stack
         (build-member config-a0 time-a0))
       (build-frame-stack
           frame-y
         (build-stack
           (build-member config-a0 time-a0)))
       (build-frame-stack-with-members
           frame-y
         (build-member config-a0 time-a0))
       stack-a)
     (assert-all-share-a-member
       stack-a
       stack-z
       frame-stack-a
       frame-stack-z
       (build-stack
         member-b0 member-a0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0))
     (assert-any-share-a-member
       stack-a
       stack-z)
     (assert-any-share-a-member
       stack-a
       frame-stack-z)
     (assert-any-share-a-member
       stack-a
       (build-stack
         member-b0 member-a0 member-c0))
     (assert-any-share-a-member
       stack-a
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0)))
     (assert-any-share-a-member
       stack-a
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0))
     (assert-any-share-a-member
       stack-a
       stack-x
       frame-stack-x
       frame-stack-b
       frame-stack-a-alt
       (build-stack
         member-b0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-c0)
       (build-stack
         (build-member config-a0 time-a0))
       (build-frame-stack
           frame-y
         (build-stack
           (build-member config-a0 time-a0)))
       (build-frame-stack-with-members
           frame-y
         (build-member config-a0 time-a0))
       frame-stack-z)
     ;; test unshared stack members
     (assert-member-unshared-by-all
         member-a0
       stack-b
       stack-x
       frame-stack-b
       frame-stack-x
       frame-stack-a-alt
       (build-stack
         member-b0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-c0)
       (build-stack
         (build-member config-a0 time-a0))
       (build-frame-stack
           frame-y
         (build-stack
           (build-member config-a0 time-a0)))
       (build-frame-stack-with-members
           frame-y
         (build-member config-a0 time-a0)))
     (assert-member-unshared-by-any
         member-a0
       stack-b)
     (assert-member-unshared-by-any
         member-a0
       stack-x)
     (assert-member-unshared-by-any
         member-a0
       frame-stack-b)
     (assert-member-unshared-by-any
         member-a0
       frame-stack-x)
     (assert-member-unshared-by-any
         member-a0
       frame-stack-a-alt)
     (assert-member-unshared-by-any
         member-a0
       (build-stack
         member-b0 member-c0))
     (assert-member-unshared-by-any
         member-a0
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-c0)))
     (assert-member-unshared-by-any
         member-a0
       (build-frame-stack-with-members
           frame-y
         member-b0 member-c0))
     (assert-member-unshared-by-any
         member-a0
       (build-stack
         (build-member config-a0 time-a0)))
     (assert-member-unshared-by-any
         member-a0
       (build-frame-stack
           frame-y
         (build-stack
           (build-member config-a0 time-a0))))
     (assert-member-unshared-by-any
         member-a0
       (build-frame-stack-with-members
           frame-y
         (build-member config-a0 time-a0)))
     (assert-member-unshared-by-any
         member-a0
       stack-a
       stack-z
       frame-stack-a
       frame-stack-z
       (build-stack
         member-b0 member-a0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0)
       stack-c)
     (assert-all-share-no-member
       stack-a
       stack-z
       frame-stack-a
       frame-stack-z
       (build-stack
         member-b0 member-a0 member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-b0 member-a0 member-c0))
       (build-frame-stack-with-members
           frame-y
         member-b0 member-a0 member-c0)
       stack-c)
     (assert-any-share-no-member
       stack-a
       stack-x
       frame-stack-b
       frame-stack-x
       frame-stack-a-alt
       (build-stack
         member-c0)
       (build-frame-stack
           frame-y
         (build-stack
           member-c1))
       (build-frame-stack-with-members
           frame-y
         member-c2)
       (build-stack
         (build-member config-a0 time-a0))
       (build-frame-stack
           frame-y
         (build-stack
           (build-member config-a0 time-a0)))
       (build-frame-stack-with-members
           frame-y
         (build-member config-a0 time-a0)))
     ;; test unshared duplicates
     (assert-deep-copy
       t
       t)
     (assert-deep-copy
       nil
       nil)
     (assert-deep-copy
       config-a0
       config-a0)
     (assert-deep-copy
       time-a0
       time-a0)
     (assert-deep-copy
       member-a0
       (build-member config-a0 time-a0))
     (assert-deep-copy
       stack-a
       (build-stack
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2)))
     (assert-deep-copy
       frame-stack-a
       (build-frame-stack
           frame-a
         (build-stack
           (build-member config-a0 time-a0)
           (build-member config-a1 time-a1)
           (build-member config-a2 time-a2))))
     (assert-deep-copy
       frame-stack-a
       (build-frame-stack-with-members
           frame-a
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2)))
     ;; test frame stack unshared duplicates
     (assert-deep-copy
       frame-stack-a
       frame-stack-a-alt)
     (assert-deep-copy
       frame-stack-b
       frame-stack-b-alt)
     (assert-deep-copy
       frame-stack-c
       frame-stack-c-alt)
     (assert-deep-copy
       frame-stack-x
       frame-stack-x-alt)
     (assert-deep-copy
       frame-stack-y
       frame-stack-y-alt)
     (assert-deep-copy
       frame-stack-z
       frame-stack-z-alt)
     ;; test the list of frame stacks with unshared duplicates
     (assert-window-stacks
       frame-stack-a-alt
       frame-stack-b-alt
       frame-stack-c-alt
       frame-stack-x-alt)
     ;; test the list of frame stacks backup (unshared duplicate)
     (backup-window-stacks)
     (assert-deep-copy
       window-stacks-bck
       resize-window--window-stacks)
     (assert-window-stacks-is-backup)
     ;; test a stack member for values
     (assert-member
         member-a0
       config-a0 time-a0)
     ;; test a stack member for a value
     (assert-member-config
         member-a0
       config-a0)
     (assert-member-svtime
         member-a0
       time-a0)
     ;; test building a stack member with values
     (assert-equal-members
       member-a0
       (build-member config-a0 time-a0))
     ;; test an empty stack for no stack members
     (assert-stack
         stack-x
       nil)
     ;; test a stack for stack members
     (assert-stack
         stack-a
       member-a0 member-a1 member-a2)
     (assert-stack
         stack-a
       (build-member config-a0 time-a0)
       (build-member config-a1 time-a1)
       (build-member config-a2 time-a2))
     ;; test building a stack with no members
     (assert-equal-stacks
       stack-x
       nil)
     (assert-equal-stacks
       stack-x
       (build-stack nil))
     ;; test building a stack with stack members
     (assert-equal-stacks
       stack-a
       (build-stack member-a0 member-a1 member-a2))
     (assert-equal-stacks
       stack-a
       (build-stack
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2)))
     ;; test a frame stack for an empty stack
     (assert-frame-stack
         frame-stack-x
       nil)
     (assert-frame-stack
         frame-stack-x
       stack-x)
     (assert-frame-stack
         frame-stack-x
       (build-stack nil))
     ;; test a frame stack for a stack
     (assert-frame-stack
         frame-stack-a
       stack-a)
     (assert-frame-stack
         frame-stack-a
       (build-stack member-a0 member-a1 member-a2))
     (assert-frame-stack
         frame-stack-a
       (build-stack
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2)))
     ;; test a frame stack for no stack members
     (assert-frame-stack-members
         frame-stack-x
       nil)
     ;; test a frame stack for stack members
     (assert-frame-stack-members
         frame-stack-a
       member-a0 member-a1 member-a2)
     (assert-frame-stack-members
         frame-stack-a
       (build-member config-a0 time-a0)
       (build-member config-a1 time-a1)
       (build-member config-a2 time-a2))
     ;; test building a frame stack with an empty stack
     (assert-equal-frame-stacks
       frame-stack-x
       (build-frame-stack
           frame-x
         nil))
     (assert-equal-frame-stacks
       frame-stack-x
       (build-frame-stack
           frame-x
         stack-x))
     (assert-equal-frame-stacks
       frame-stack-x
       (build-frame-stack
           frame-x
         (build-stack nil)))
     ;; test building a frame stack with a stack
     (assert-equal-frame-stacks
       frame-stack-a
       (build-frame-stack
           frame-a
         stack-a))
     (assert-equal-frame-stacks
       frame-stack-a
       (build-frame-stack
           frame-a
         (build-stack member-a0 member-a1 member-a2)))
     (assert-equal-frame-stacks
       frame-stack-a
       (build-frame-stack
           frame-a
         (build-stack
           (build-member config-a0 time-a0)
           (build-member config-a1 time-a1)
           (build-member config-a2 time-a2))))
     ;; test building a frame stack with no members
     (assert-equal-frame-stacks
       frame-stack-x
       (build-frame-stack-with-members
           frame-x
         nil))
     ;; test building a frame stack with stack members
     (assert-equal-frame-stacks
       frame-stack-a
       (build-frame-stack-with-members
           frame-a
         member-a0 member-a1 member-a2))
     (assert-equal-frame-stacks
       frame-stack-a
       (build-frame-stack-with-members
           frame-a
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2)))
     ;; test the list of frame stacks for frame stacks
     (assert-window-stacks
       frame-stack-a
       frame-stack-b
       frame-stack-c
       frame-stack-x)
     ;; test building the list of frame stacks with stacks
     (assert-window-stacks
       (build-frame-stack
           frame-a
         stack-a)
       (build-frame-stack
           frame-b
         stack-b)
       (build-frame-stack
           frame-c
         stack-c)
       (build-frame-stack
           frame-x
         nil))
     (assert-window-stacks
       (build-frame-stack
           frame-a
         stack-a)
       (build-frame-stack
           frame-b
         stack-b)
       (build-frame-stack
           frame-c
         stack-c)
       (build-frame-stack
           frame-x
         stack-x))
     (assert-window-stacks
       (build-frame-stack
           frame-a
         (build-stack member-a0 member-a1 member-a2))
       (build-frame-stack
           frame-b
         (build-stack member-b0 member-b1 member-b2))
       (build-frame-stack
           frame-c
         (build-stack member-c0 member-c1 member-c2))
       (build-frame-stack
           frame-x
         (build-stack nil)))
     (assert-window-stacks
       (build-frame-stack
           frame-a
         (build-stack
           (build-member config-a0 time-a0)
           (build-member config-a1 time-a1)
           (build-member config-a2 time-a2)))
       (build-frame-stack
           frame-b
         (build-stack
           (build-member config-b0 time-b0)
           (build-member config-b1 time-b1)
           (build-member config-b2 time-b2)))
       (build-frame-stack
           frame-c
         (build-stack
           (build-member config-c0 time-c0)
           (build-member config-c1 time-c1)
           (build-member config-c2 time-c2)))
       (build-frame-stack
           frame-x
         (build-stack nil)))
     ;; test building the list of frame stacks with stack members
     (assert-window-stacks
       (build-frame-stack-with-members
           frame-a
         member-a0 member-a1 member-a2)
       (build-frame-stack-with-members
           frame-b
         member-b0 member-b1 member-b2)
       (build-frame-stack-with-members
           frame-c
         member-c0 member-c1 member-c2)
       (build-frame-stack-with-members
           frame-x
         nil))
     (assert-window-stacks
       (build-frame-stack-with-members
           frame-a
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2))
       (build-frame-stack-with-members
           frame-b
         (build-member config-b0 time-b0)
         (build-member config-b1 time-b1)
         (build-member config-b2 time-b2))
       (build-frame-stack-with-members
           frame-c
         (build-member config-c0 time-c0)
         (build-member config-c1 time-c1)
         (build-member config-c2 time-c2))
       (build-frame-stack-with-members
           frame-x
         nil))
     ;; test customizing the list of frame stacks
     (build-window-stacks
       frame-stack-a frame-stack-b frame-stack-c frame-stack-x)
     (assert-window-stacks
       frame-stack-a frame-stack-b frame-stack-c frame-stack-x)
     (build-window-stacks
       frame-stack-a frame-stack-x)
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     (build-window-stacks
       nil)
     (assert-window-stacks
       nil)
     ;; test customizing the list of frame stacks with stacks
     (build-window-stacks
       (build-frame-stack
           frame-a
         stack-a)
       (build-frame-stack
           frame-x
         nil))
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     (build-window-stacks
       (build-frame-stack
           frame-a
         stack-a)
       (build-frame-stack
           frame-x
         stack-x))
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     (build-window-stacks
       (build-frame-stack
           frame-a
         (build-stack member-a0 member-a1 member-a2))
       (build-frame-stack
           frame-x
         (build-stack nil)))
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     (assert-window-stacks
       (build-frame-stack
           frame-a
         (build-stack
           (build-member config-a0 time-a0)
           (build-member config-a1 time-a1)
           (build-member config-a2 time-a2)))
       (build-frame-stack
           frame-x
         (build-stack nil)))
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     ;; test customizing the list of frame stacks with stack members
     (build-window-stacks
       (build-frame-stack-with-members
           frame-a
         member-a0 member-a1 member-a2)
       (build-frame-stack-with-members
           frame-x
         nil))
     (assert-window-stacks
       frame-stack-a frame-stack-x)
     (build-window-stacks
       (build-frame-stack-with-members
           frame-a
         (build-member config-a0 time-a0)
         (build-member config-a1 time-a1)
         (build-member config-a2 time-a2))
       (build-frame-stack-with-members
           frame-x
         nil))
     (assert-window-stacks
       frame-stack-a frame-stack-x)))

(ert-deftest resize-window--test-dummy-stacks ()
  "Test the dummy list of frame stacks.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    (assert-window-stacks-is-dummy)
    (assert-dummy-window-stacks)))

(ert-deftest resize-window--test-frame-stack ()
  "Test `resize-window--frame-stack':
- should switch a nil frame argument with the selected frame;
- should return the first frame stack matching a frame;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: get frame stack of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--frame-stack nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: get frame stack of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--frame-stack frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      frame-stack-x           ;; 1st `frame-x' match
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z)))
    (backup-window-stacks)
    ;; get frame stack of nil frame (selected frame: frame z)
    ;; -- get frame stack of frame z (1st match) --
    (assert-result
        (resize-window--frame-stack nil)
      frame-stack-z)
    (assert-window-stacks-is-backup)
    ;; get frame stack of frame z (1st match)
    (assert-result
        (resize-window--frame-stack frame-z)
      frame-stack-z)
    (assert-window-stacks-is-backup)
    ;; get frame stack of frame x (1st match)
    (assert-result
        (resize-window--frame-stack frame-x)
      frame-stack-x)
    (assert-window-stacks-is-backup)
    ;; get frame stack of frame a (1st match)
    (assert-result
        (resize-window--frame-stack frame-a)
      frame-stack-a)
    (assert-window-stacks-is-backup)))

(ert-deftest resize-window--test-del-frame-stack ()
  "Test `resize-window--del-frame-stack':
- should switch a nil frame argument with the selected frame;
- should delete the first frame stack matching a frame;
- should delete duplicate objects `eq' to frame stack;
- should return the deleted frame stack object;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: delete frame stack of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--del-frame-stack nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: delete frame stack of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--del-frame-stack frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      frame-stack-x           ;; 1st `frame-x' match
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z))
      frame-stack-a           ;; 3rd `frame-a' match
      frame-stack-x           ;; 3rd `frame-x' match
      frame-stack-z           ;; 3rd `frame-z' match
      frame-stack-a-alt       ;; 4th `frame-a' match
      frame-stack-x-alt       ;; 4th `frame-x' match
      frame-stack-z-alt)      ;; 4th `frame-z' match
    (backup-window-stacks)
    ;; delete frame stack of nil frame (selected frame: frame z)
    ;; -- delete frame stack of frame z (1st and 3rd `eq' match) --
    (assert-result
        (resize-window--del-frame-stack nil)
      frame-stack-z)
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; delete frame stack of frame x (1st and 3rd `eq' match)
    (assert-result
        (resize-window--del-frame-stack frame-x)
      frame-stack-x)
    (assert-window-stacks
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; delete frame stack of frame a (1st and 3rd `eq' match)
    (assert-result
        (resize-window--del-frame-stack frame-a)
      frame-stack-a)
    (assert-window-stacks
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; delete frame stack of frame z (2nd and 4th `eq' match)
    (assert-result
        (resize-window--del-frame-stack frame-z)
      frame-stack-z-alt)
    (assert-window-stacks
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; delete frame stack of frame x (2nd and 4th `eq' match)
    (assert-result
        (resize-window--del-frame-stack frame-x)
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-a-alt
      frame-stack-a-alt)
    ;; delete frame stack of frame a (2nd and 4th `eq' match)
    (assert-result
        (resize-window--del-frame-stack frame-a)
      frame-stack-a-alt)
    (assert-window-stacks
      nil)))

(ert-deftest resize-window--test-get-stack-head ()
  "Test `resize-window--get-stack-head':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should return the frame stack first member;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: get member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--get-stack-head nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: get member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--get-stack-head frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      (setq frame-stack-a     ;; 1st `frame-a' match
            (build-frame-stack-with-members
                frame-a
              member-a0 member-a1))
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              nil))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1))
      (setq frame-stack-a-alt ;; 2nd `frame-a' match
            (build-frame-stack-with-members
                frame-a
              member-a2))
      (setq frame-stack-x-alt ;; 2nd `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-b0))
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (build-frame-stack-with-members
                frame-z
              member-c2)))
    (backup-window-stacks)
    ;; review shared/unshared members
    (assert-any-share-no-member
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; get member of nil frame (selected frame: frame z)
    ;; -- get member of frame stack z (1st match) --
    (assert-result
        (resize-window--get-stack-head nil)
      member-c0)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack z (1st match)
    (assert-result
        (resize-window--get-stack-head frame-z)
      member-c0)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack x (1st match)
    (assert-result
        (resize-window--get-stack-head frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack a (1st match)
    (assert-result
        (resize-window--get-stack-head frame-a)
      member-a0)
    (assert-window-stacks-is-backup)))

(ert-deftest resize-window--test-pop-stack-head ()
  "Test `resize-window--pop-stack-head'.
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should pop the first member of the frame stack;
- should drop the frame stack if ended up empty;
- should drop also objects `eq' to frame stack;
- should return the popped up stack member;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: pop member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--pop-stack-head nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: pop member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--pop-stack-head frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              nil))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1))
      (setq frame-stack-x-alt ;; 2nd `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-b0))
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (build-frame-stack-with-members
                frame-z
              member-c1))
      frame-stack-x           ;; 3rd `frame-x' match
      frame-stack-z           ;; 3rd `frame-z' match
      frame-stack-x-alt       ;; 4th `frame-x' match
      frame-stack-z-alt)      ;; 4th `frame-z' match
    (backup-window-stacks)
    ;; review shared/unshared members
    (assert-member-shared-by-all
        member-c1
      frame-stack-z
      frame-stack-z-alt)
    (assert-any-share-no-member
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt)
    ;; pop member of nil frame (selected frame: frame z)
    ;; -- pop member of frame stack z (1st and 3rd `eq' match) --
    (assert-result
        (resize-window--pop-stack-head nil)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      member-c1)
    (assert-window-stacks
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack z (1st and 3rd `eq' match)
    ;; -- drop empty frame stack z --
    (assert-result
        (resize-window--pop-stack-head frame-z)
      member-c1)
    (assert-frame-stack-members
        frame-stack-z
      nil)
    (assert-window-stacks
      frame-stack-x
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack x (1st and 3rd `eq' match)
    ;; -- drop empty frame stack x --
    (assert-result
        (resize-window--pop-stack-head frame-x)
      nil)
    (assert-frame-stack-members
        frame-stack-x
      nil)
    (assert-window-stacks
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack z (2nd and 4th `eq' match)
    ;; -- drop empty frame stack z --
    (assert-result
        (resize-window--pop-stack-head frame-z)
      member-c1)
    (assert-frame-stack-members
        frame-stack-z-alt
      nil)
    (assert-window-stacks
      frame-stack-x-alt
      frame-stack-x-alt)
    ;; pop member of frame stack x (2nd and 4th `eq' match)
    ;; -- drop empty frame stack x --
    (assert-result
        (resize-window--pop-stack-head frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-window-stacks
      nil)))

(ert-deftest resize-window--test-push-stack-head ()
  "Test `resize-window--push-stack-head':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should create a frame stack for a new/unsaved frame;
- should push a member first to the frame stack;
- should allow pushing duplicate members;
- should not allow to push a nil member;
- should return the member pushed.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: push nil member to nil frame (unsaved selected frame)
    (assert-result
        (resize-window--push-stack-head nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--push-stack-head nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to saved frame x (empty stack)
    (assert-result
        (resize-window--push-stack-head nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to saved frame a
    (assert-result
        (resize-window--push-stack-head nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a      ;; 1st `frame-a' match
      frame-stack-x      ;; 1st `frame-x' match
      frame-stack-a-alt  ;; 2nd `frame-a' match
      frame-stack-x-alt) ;; 2nd `frame-x' match
    (backup-window-stacks)
    ;; push member to nil frame (unsaved selected frame)
    ;; -- create new frame stack z at 1st position --
    (assert-result
        ;; -- push unique member c1 --
        (resize-window--push-stack-head member-c1 nil)
      member-c1)
    (assert-frame-stack-members
        (setq frame-stack-z (car resize-window--window-stacks))
      member-c1)
    (setq frame-z (car frame-stack-z))
    (assert-selected-frame frame-stack-z)
    (assert-member-shared-by-all
        member-c1
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c1
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to created frame stack z
    (assert-result
        ;; -- push unique member c0 --
        (resize-window--push-stack-head member-c0 frame-z)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to new/unsaved frame y
    ;; -- create new frame stack y at 1st position --
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--push-stack-head member-a0 frame-y)
      member-a0)
    (assert-frame-stack-members
        (setq frame-stack-y (car resize-window--window-stacks))
      member-a0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to frame stack x (1st match)
    (assert-result
        ;; -- push unique member b0 to empty stack --
        (resize-window--push-stack-head member-b0 frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-member-shared-by-all
        member-b0
      frame-stack-x)
    (assert-member-unshared-by-all
        member-b0
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to frame stack a (1st match)
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--push-stack-head member-a0 frame-a)
      member-a0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a0 member-a1 member-a2)
    (assert-frame-stack-members
        frame-stack-a-alt
      member-a0 member-a1 member-a2)
    (assert-result
        (nth 0 (setq stack-a (cdr frame-stack-a)))
      (nth 1 stack-a))
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)))

(ert-deftest resize-window--test-set-stack-head ()
  "Test `resize-window--set-stack-head':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should create a frame stack for a new/unsaved frame;
- should set the first member of the frame stack;
- should push a first member in an empty stack;
- should allow to set members as duplicates;
- should not allow to set a member as nil;
- should return the member set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--set-stack-head nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--set-stack-head nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of saved frame x (empty stack)
    (assert-result
        (resize-window--set-stack-head nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of saved frame a
    (assert-result
        (resize-window--set-stack-head nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a      ;; 1st `frame-a' match
      frame-stack-x      ;; 1st `frame-x' match
      frame-stack-a-alt  ;; 2nd `frame-a' match
      frame-stack-x-alt) ;; 2nd `frame-x' match
    (backup-window-stacks)
    ;; set member of nil frame (unsaved selected frame)
    ;; -- create new frame stack z at 1st position --
    (assert-result
        ;; -- push unique member c1 --
        (resize-window--set-stack-head member-c1 nil)
      member-c1)
    (assert-frame-stack-members
        (setq frame-stack-z (car resize-window--window-stacks))
      member-c1)
    (setq frame-z (car frame-stack-z))
    (assert-selected-frame frame-stack-z)
    (assert-member-shared-by-all
        member-c1
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c1
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of created frame stack z
    (assert-result
        ;; -- set unique member c0 --
        (resize-window--set-stack-head member-c0 frame-z)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of new/unsaved frame y
    ;; -- create new frame stack y at 1st position --
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--set-stack-head member-a0 frame-y)
      member-a0)
    (assert-frame-stack-members
        (setq frame-stack-y (car resize-window--window-stacks))
      member-a0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of frame stack x (1st match)
    (assert-result
        ;; -- push unique member b0 to empty stack --
        (resize-window--set-stack-head member-b0 frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-member-shared-by-all
        member-b0
      frame-stack-x)
    (assert-member-unshared-by-all
        member-b0
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of frame stack a (1st match)
    (assert-result
        ;; -- set duplicate of member a1 --
        (resize-window--set-stack-head member-a1 frame-a)
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a1 member-a1 member-a2)
    (assert-frame-stack-members
        frame-stack-a-alt
      member-a0 member-a1 member-a2)
    (assert-result
        (nth 0 (setq stack-a (cdr frame-stack-a)))
      (nth 1 stack-a))
    (assert-member-shared-by-all
        member-a1
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-y
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)))

(ert-deftest resize-window--test-stack-head-config ()
  "Test `resize-window--stack-head-config':
- should switch a nil frame argument with the selected frame;
- should not create frame stacks for new/unsaved frames;
- should select the first frame stack matching a frame;
- should select the first member of the frame stack;
- should not set a configuration of an empty stack;
- should set the frame stack member configuration;
- should allow to set duplicate configurations;
- should not set a configuration as nil;
- should return the configuration set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil config of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-head-config nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-head-config nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-head-config nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of saved frame a
    (assert-result
        (resize-window--stack-head-config nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-head-config config-c0 nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-head-config config-c1 frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-head-config config-c2 frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-a1))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z)))
    (backup-window-stacks)
    ;; set config of nil frame (selected frame: frame z)
    ;; -- set config of frame stack z (1st match) --
    (assert-result
        ;; -- set duplicate of config c1 --
        (resize-window--stack-head-config config-c1 nil)
      config-c1)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c0
      config-c1 time-c0)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c1 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack z (1st match)
    (assert-result
        ;; -- set duplicate of config a0 --
        (resize-window--stack-head-config config-a0 frame-z)
      config-a0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c0
      config-a0 time-c0)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-a0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack x (1st match)
    ;; -- modify shared member a1 --
    (assert-result
        ;; -- set duplicate of config a0 --
        (resize-window--stack-head-config config-a0 frame-x)
      config-a0)
    (assert-frame-stack-members
        frame-stack-x
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a1
      config-a0 time-a1)
    (assert-member-shared-by-all
        member-a1
      frame-stack-a
      frame-stack-x)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-x
      (build-member config-a0 time-a1))
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a0 time-a1)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack a (1st match)
    (assert-result
        ;; -- set unique config b0 --
        (resize-window--stack-head-config config-b0 frame-a)
      config-b0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a0
      config-b0 time-a0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-b0 time-a0)
      (build-member config-a0 time-a1)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)))

(ert-deftest resize-window--test-stack-head-svtime ()
  "Test `resize-window--stack-head-svtime':
- should switch a nil frame argument with the selected frame;
- should not create frame stacks for new/unsaved frames;
- should select the first frame stack matching a frame;
- should select the first member of the frame stack;
- should not set a saved time of an empty stack;
- should set the frame stack member saved time;
- should allow to set duplicate saved times;
- should not set a saved time as nil;
- should return the saved time set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil time of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-head-svtime nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-head-svtime nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-head-svtime nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of saved frame a
    (assert-result
        (resize-window--stack-head-svtime nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-head-svtime time-c0 nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-head-svtime time-c1 frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-head-svtime time-c2 frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-a1))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z)))
    (backup-window-stacks)
    ;; set time of nil frame (selected frame: frame z)
    ;; -- set time of frame stack z (1st match) --
    (assert-result
        ;; -- set duplicate of time c1 --
        (resize-window--stack-head-svtime time-c1 nil)
      time-c1)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c0
      config-c0 time-c1)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-c1)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack z (1st match)
    (assert-result
        ;; -- set duplicate of time a0 --
        (resize-window--stack-head-svtime time-a0 frame-z)
      time-a0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c0
      config-c0 time-a0)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-a0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack x (1st match)
    ;; -- modify shared member a1 --
    (assert-result
        ;; -- set duplicate of time a0 --
        (resize-window--stack-head-svtime time-a0 frame-x)
      time-a0)
    (assert-frame-stack-members
        frame-stack-x
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a1
      config-a1 time-a0)
    (assert-member-shared-by-all
        member-a1
      frame-stack-a
      frame-stack-x)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-x
      (build-member config-a1 time-a0))
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a0)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack a (1st match)
    (assert-result
        ;; -- set unique time b0 --
        (resize-window--stack-head-svtime time-b0 frame-a)
      time-b0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a0
      config-a0 time-b0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-b0)
      (build-member config-a1 time-a0)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)))

(ert-deftest resize-window--test-get-stack-tail ()
  "Test `resize-window--get-stack-tail':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should return the frame stack last member;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: get member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--get-stack-tail nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: get member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--get-stack-tail frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      (setq frame-stack-a     ;; 1st `frame-a' match
            (build-frame-stack-with-members
                frame-a
              member-a0 member-a1))
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              nil))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1))
      (setq frame-stack-a-alt ;; 2nd `frame-a' match
            (build-frame-stack-with-members
                frame-a
              member-a2))
      (setq frame-stack-x-alt ;; 2nd `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-b0))
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (build-frame-stack-with-members
                frame-z
              member-c2)))
    (backup-window-stacks)
    ;; review shared/unshared members
    (assert-any-share-no-member
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; get member of nil frame (selected frame: frame z)
    ;; -- get member of frame stack z (1st match) --
    (assert-result
        (resize-window--get-stack-tail nil)
      member-c1)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack z (1st match)
    (assert-result
        (resize-window--get-stack-tail frame-z)
      member-c1)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack x (1st match)
    (assert-result
        (resize-window--get-stack-tail frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; get member of frame stack a (1st match)
    (assert-result
        (resize-window--get-stack-tail frame-a)
      member-a1)
    (assert-window-stacks-is-backup)))

(ert-deftest resize-window--test-pop-stack-tail ()
  "Test `resize-window--pop-stack-tail'.
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should pop the last member of the frame stack;
- should drop the frame stack if ended up empty;
- should drop also objects `eq' to frame stack;
- should return the popped up stack member;
- should return nil for new/unsaved frames.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: pop member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--pop-stack-tail nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: pop member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--pop-stack-tail frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              nil))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1))
      (setq frame-stack-x-alt ;; 2nd `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-b0))
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (build-frame-stack-with-members
                frame-z
              member-c1))
      frame-stack-x           ;; 3rd `frame-x' match
      frame-stack-z           ;; 3rd `frame-z' match
      frame-stack-x-alt       ;; 4th `frame-x' match
      frame-stack-z-alt)      ;; 4th `frame-z' match
    (backup-window-stacks)
    ;; review shared/unshared members
    (assert-member-shared-by-all
        member-c1
      frame-stack-z
      frame-stack-z-alt)
    (assert-any-share-no-member
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt)
    ;; pop member of nil frame (selected frame: frame z)
    ;; -- pop member of frame stack z (1st and 3rd `eq' match) --
    (assert-result
        (resize-window--pop-stack-tail nil)
      member-c1)
    (assert-frame-stack-members
        frame-stack-z
      member-c0)
    (assert-window-stacks
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x
      frame-stack-z
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack z (1st and 3rd `eq' match)
    ;; -- drop empty frame stack z --
    (assert-result
        (resize-window--pop-stack-tail frame-z)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      nil)
    (assert-window-stacks
      frame-stack-x
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack x (1st and 3rd `eq' match)
    ;; -- drop empty frame stack x --
    (assert-result
        (resize-window--pop-stack-tail frame-x)
      nil)
    (assert-frame-stack-members
        frame-stack-x
      nil)
    (assert-window-stacks
      frame-stack-x-alt
      frame-stack-z-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; pop member of frame stack z (2nd and 4th `eq' match)
    ;; -- drop empty frame stack z --
    (assert-result
        (resize-window--pop-stack-tail frame-z)
      member-c1)
    (assert-frame-stack-members
        frame-stack-z-alt
      nil)
    (assert-window-stacks
      frame-stack-x-alt
      frame-stack-x-alt)
    ;; pop member of frame stack x (2nd and 4th `eq' match)
    ;; -- drop empty frame stack x --
    (assert-result
        (resize-window--pop-stack-tail frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-window-stacks
      nil)))

(ert-deftest resize-window--test-push-stack-tail ()
  "Test `resize-window--push-stack-tail':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should create a frame stack for a new/unsaved frame;
- should push a member last to the frame stack;
- should allow pushing duplicate members;
- should not allow to push a nil member;
- should return the member pushed.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: push nil member to nil frame (unsaved selected frame)
    (assert-result
        (resize-window--push-stack-tail nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--push-stack-tail nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to saved frame x (empty stack)
    (assert-result
        (resize-window--push-stack-tail nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: push nil member to saved frame a
    (assert-result
        (resize-window--push-stack-tail nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a      ;; 1st `frame-a' match
      frame-stack-x      ;; 1st `frame-x' match
      frame-stack-a-alt  ;; 2nd `frame-a' match
      frame-stack-x-alt) ;; 2nd `frame-x' match
    (backup-window-stacks)
    ;; push member to nil frame (unsaved selected frame)
    ;; -- create new frame stack z at 1st position --
    (assert-result
        ;; -- push unique member c1 --
        (resize-window--push-stack-tail member-c1 nil)
      member-c1)
    (assert-frame-stack-members
        (setq frame-stack-z (car resize-window--window-stacks))
      member-c1)
    (setq frame-z (car frame-stack-z))
    (assert-selected-frame frame-stack-z)
    (assert-member-shared-by-all
        member-c1
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c1
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to created frame stack z
    (assert-result
        ;; -- push unique member c0 --
        (resize-window--push-stack-tail member-c0 frame-z)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      member-c1 member-c0)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to new/unsaved frame y
    ;; -- create new frame stack y at 1st position --
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--push-stack-tail member-a0 frame-y)
      member-a0)
    (assert-frame-stack-members
        (setq frame-stack-y (car resize-window--window-stacks))
      member-a0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to frame stack x (1st match)
    (assert-result
        ;; -- push unique member b0 to empty stack --
        (resize-window--push-stack-tail member-b0 frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-member-shared-by-all
        member-b0
      frame-stack-x)
    (assert-member-unshared-by-all
        member-b0
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; push member to frame stack a (1st match)
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--push-stack-tail member-a0 frame-a)
      member-a0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2 member-a0)
    (assert-frame-stack-members
        frame-stack-a-alt
      member-a0 member-a1 member-a2)
    (assert-result
        (nth 3 (setq stack-a (cdr frame-stack-a)))
      (nth 0 stack-a))
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)))

(ert-deftest resize-window--test-set-stack-tail ()
  "Test `resize-window--set-stack-tail':
- should switch a nil frame argument with the selected frame;
- should select the first frame stack matching a frame;
- should create a frame stack for a new/unsaved frame;
- should set the last member of the frame stack;
- should push a first member in an empty stack;
- should allow to set members as duplicates;
- should not allow to set a member as nil;
- should return the member set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil member of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--set-stack-tail nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--set-stack-tail nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of saved frame x (empty stack)
    (assert-result
        (resize-window--set-stack-tail nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil member of saved frame a
    (assert-result
        (resize-window--set-stack-tail nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a      ;; 1st `frame-a' match
      frame-stack-x      ;; 1st `frame-x' match
      frame-stack-a-alt  ;; 2nd `frame-a' match
      frame-stack-x-alt) ;; 2nd `frame-x' match
    (backup-window-stacks)
    ;; set member of nil frame (unsaved selected frame)
    ;; -- create new frame stack z at 1st position --
    (assert-result
        ;; -- push unique member c1 --
        (resize-window--set-stack-tail member-c1 nil)
      member-c1)
    (assert-frame-stack-members
        (setq frame-stack-z (car resize-window--window-stacks))
      member-c1)
    (setq frame-z (car frame-stack-z))
    (assert-selected-frame frame-stack-z)
    (assert-member-shared-by-all
        member-c1
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c1
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of created frame stack z
    (assert-result
        ;; -- set unique member c0 --
        (resize-window--set-stack-tail member-c0 frame-z)
      member-c0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0)
    (assert-member-shared-by-all
        member-c0
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c0
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of new/unsaved frame y
    ;; -- create new frame stack y at 1st position --
    (assert-result
        ;; -- push duplicate of member a0 --
        (resize-window--set-stack-tail member-a0 frame-y)
      member-a0)
    (assert-frame-stack-members
        (setq frame-stack-y (car resize-window--window-stacks))
      member-a0)
    (assert-member-shared-by-all
        member-a0
      frame-stack-a
      frame-stack-y)
    (assert-member-unshared-by-all
        member-a0
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of frame stack x (1st match)
    (assert-result
        ;; -- push unique member b0 to empty stack --
        (resize-window--set-stack-tail member-b0 frame-x)
      member-b0)
    (assert-frame-stack-members
        frame-stack-x
      member-b0)
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-member-shared-by-all
        member-b0
      frame-stack-x)
    (assert-member-unshared-by-all
        member-b0
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    ;; set member of frame stack a (1st match)
    (assert-result
        ;; -- set duplicate of member a1 --
        (resize-window--set-stack-tail member-a1 frame-a)
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a1)
    (assert-frame-stack-members
        frame-stack-a-alt
      member-a0 member-a1 member-a2)
    (assert-result
        (nth 2 (setq stack-a (cdr frame-stack-a)))
      (nth 1 stack-a))
    (assert-member-shared-by-all
        member-a1
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-y
      frame-stack-z
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)
    (assert-window-stacks
      frame-stack-y
      frame-stack-z
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt)))

(ert-deftest resize-window--test-stack-tail-config ()
  "Test `resize-window--stack-tail-config':
- should switch a nil frame argument with the selected frame;
- should not create frame stacks for new/unsaved frames;
- should select the first frame stack matching a frame;
- should select the last member of the frame stack;
- should not set a configuration of an empty stack;
- should set the frame stack member configuration;
- should allow to set duplicate configurations;
- should not set a configuration as nil;
- should return the configuration set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil config of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-tail-config nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-tail-config nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-tail-config nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil config of saved frame a
    (assert-result
        (resize-window--stack-tail-config nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-tail-config config-c0 nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-tail-config config-c1 frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set config of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-tail-config config-c2 frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-a1))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z)))
    (backup-window-stacks)
    ;; set config of nil frame (selected frame: frame z)
    ;; -- set config of frame stack z (1st match) --
    (assert-result
        ;; -- set duplicate of config c1 --
        (resize-window--stack-tail-config config-c1 nil)
      config-c1)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c2
      config-c1 time-c2)
    (assert-member-shared-by-all
        member-c2
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c2
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c1 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack z (1st match)
    (assert-result
        ;; -- set duplicate of config a0 --
        (resize-window--stack-tail-config config-a0 frame-z)
      config-a0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c2
      config-a0 time-c2)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-a0 time-c2))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack x (1st match)
    ;; -- modify shared member a1 --
    (assert-result
        ;; -- set duplicate of config a0 --
        (resize-window--stack-tail-config config-a0 frame-x)
      config-a0)
    (assert-frame-stack-members
        frame-stack-x
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a1
      config-a0 time-a1)
    (assert-member-shared-by-all
        member-a1
      frame-stack-a
      frame-stack-x)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-x
      (build-member config-a0 time-a1))
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a0 time-a1)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set config of frame stack a (1st match)
    (assert-result
        ;; -- set unique config b0 --
        (resize-window--stack-tail-config config-b0 frame-a)
      config-b0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a2
      config-b0 time-a2)
    (assert-member-shared-by-all
        member-a2
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a2
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a0 time-a1)
      (build-member config-b0 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)))

(ert-deftest resize-window--test-stack-tail-svtime ()
  "Test `resize-window--stack-tail-svtime':
- should switch a nil frame argument with the selected frame;
- should not create frame stacks for new/unsaved frames;
- should select the first frame stack matching a frame;
- should select the last member of the frame stack;
- should not set a saved time of an empty stack;
- should set the frame stack member saved time;
- should allow to set duplicate saved times;
- should not set a saved time as nil;
- should return the saved time set.

See bindings in `resize-window--bind-dummy-stacks'.
See stack macros usage in `assert-dummy-window-stacks'."
  (resize-window--bind-dummy-stacks
    ;; NOTES:
    ;; - `frame-stack-a-alt'-`z-alt': start as unshared duplicates.
    ;; - `frame-stack-x': starts with an empty `stack-x'.
    ;; - `frame-stack-y': null, reserved to fake frame.
    ;; - `frame-stack-z': null, reserved to true frame.
    ;; - `frame-y': starts as a new/unsaved fake frame.
    ;; - `frame-z': starts nil, reserved to true frame.
    (assert-window-stacks-is-dummy)
    ;; try: set nil time of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-tail-svtime nil nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-tail-svtime nil frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-tail-svtime nil frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set nil time of saved frame a
    (assert-result
        (resize-window--stack-tail-svtime nil frame-a)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of nil frame (unsaved selected frame)
    (assert-result
        (resize-window--stack-tail-svtime time-c0 nil)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of new/unsaved frame y (fake frame)
    (assert-result
        (resize-window--stack-tail-svtime time-c1 frame-y)
      nil)
    (assert-window-stacks-is-backup)
    ;; try: set time of saved frame x (empty stack)
    (assert-result
        (resize-window--stack-tail-svtime time-c2 frame-x)
      nil)
    (assert-window-stacks-is-backup)
    ;; force a list of frame stacks with duplicates
    (build-window-stacks
      frame-stack-a           ;; 1st `frame-a' match
      (setq frame-stack-x     ;; 1st `frame-x' match
            (build-frame-stack-with-members
                frame-x
              member-a1))
      (setq frame-stack-z     ;; 1st `frame-z' match
            (build-frame-stack-with-members
                (setq frame-z (selected-frame))
              member-c0 member-c1 member-c2))
      frame-stack-a-alt       ;; 2nd `frame-a' match
      frame-stack-x-alt       ;; 2nd `frame-x' match
      (setq frame-stack-z-alt ;; 2nd `frame-z' match
            (copy-tree frame-stack-z)))
    (backup-window-stacks)
    ;; set time of nil frame (selected frame: frame z)
    ;; -- set time of frame stack z (1st match) --
    (assert-result
        ;; -- set duplicate of time c1 --
        (resize-window--stack-tail-svtime time-c1 nil)
      time-c1)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c2
      config-c2 time-c1)
    (assert-member-shared-by-all
        member-c2
      frame-stack-z)
    (assert-member-unshared-by-all
        member-c2
      frame-stack-a
      frame-stack-x
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c1))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack z (1st match)
    (assert-result
        ;; -- set duplicate of time a0 --
        (resize-window--stack-tail-svtime time-a0 frame-z)
      time-a0)
    (assert-frame-stack-members
        frame-stack-z
      member-c0 member-c1 member-c2)
    (assert-member
        member-c2
      config-c2 time-a0)
    (assert-frame-stack-members
        frame-stack-z
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-a0))
    (assert-frame-stack-members
        frame-stack-z-alt
      (build-member config-c0 time-c0)
      (build-member config-c1 time-c1)
      (build-member config-c2 time-c2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack x (1st match)
    ;; -- modify shared member a1 --
    (assert-result
        ;; -- set duplicate of time a0 --
        (resize-window--stack-tail-svtime time-a0 frame-x)
      time-a0)
    (assert-frame-stack-members
        frame-stack-x
      member-a1)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a1
      config-a1 time-a0)
    (assert-member-shared-by-all
        member-a1
      frame-stack-a
      frame-stack-x)
    (assert-member-unshared-by-all
        member-a1
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-x
      (build-member config-a1 time-a0))
    (assert-frame-stack-members
        frame-stack-x-alt
      nil)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a0)
      (build-member config-a2 time-a2))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    ;; set time of frame stack a (1st match)
    (assert-result
        ;; -- set unique time b0 --
        (resize-window--stack-tail-svtime time-b0 frame-a)
      time-b0)
    (assert-frame-stack-members
        frame-stack-a
      member-a0 member-a1 member-a2)
    (assert-member
        member-a2
      config-a2 time-b0)
    (assert-member-shared-by-all
        member-a2
      frame-stack-a)
    (assert-member-unshared-by-all
        member-a2
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)
    (assert-frame-stack-members
        frame-stack-a
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a0)
      (build-member config-a2 time-b0))
    (assert-frame-stack-members
        frame-stack-a-alt
      (build-member config-a0 time-a0)
      (build-member config-a1 time-a1)
      (build-member config-a2 time-a2))
    (assert-window-stacks
      frame-stack-a
      frame-stack-x
      frame-stack-z
      frame-stack-a-alt
      frame-stack-x-alt
      frame-stack-z-alt)))
