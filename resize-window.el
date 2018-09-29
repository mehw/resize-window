;;; resize-window.el --- Easily resize windows          -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: Dan Sutton  <danielsutton01@gmail.com>
;; Author: Matthew White  <mehw.is.me@inventati.org>
;; Maintainer: Dan Sutton  <danielsutton01@gmail.com>
;; URL: https://github.com/dpsutton/resize-mode

;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Keywords: window, resize

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Easily allows you to resize windows.  Rather than guessing that you
;; want `C-u 17 C-x {`, you could just press FFff, which resizes 5
;; lines, then 5 lines, then one and then one.  The idea is that the
;; normal motions n,p,f,b along with e for even and w for cycling
;; windows allows for super simple resizing of windows.  All of this is
;; inside of a while loop so that you don't have to invoke more chords
;; to resize again, but just keep using standard motions until you are
;; happy.

;; Just run `M-x resize-window`. There are only a few commands to learn,
;; and they mimic the normal motions in emacs.

;;   n : Resize the window vertically like scrolling down.
;;        N  for 5 lines at once.
;;   p : Resize the window vertically like scrolling up.
;;        P  for 5 lines at once.
;;   f : Resize the window horizontally like scrolling forward.
;;        F  for 5 lines at once.
;;   b : Resize the window horizontally like scrolling backward.
;;        B  for 5 lines at once.
;;   w : Cycle through windows so that you can adjust other window panes.
;;        W  cycle in the opposite direction.
;;   e : Even the size of the windows.
;;   2 : Split the window horizontally.
;;   3 : Split the window vertically.
;;   0 : Delete the current window.
;;   k : Delete other windows and save the state on the stack.
;;   x : Drop the current saved state. Switch to another one.
;;   s : Save the state on the stack so you may restore it later.
;;   > : Restore to a previous saved state.
;;        <  Restore in the opposite direction.
;;   ? : Display the help menu listing commands.


;;; Code:

(require 'cl-lib)

(defgroup resize-window nil
  "Quickly resize windows."
  :group 'convenience
  :prefix "rw-")

(defcustom resize-window-coarse-argument 5
  "Set how big a capital letter movement is."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-fine-argument 1
  "Set how big the default movement should be."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-allow-backgrounds t
  "Allow resize mode to set backgrounds.
This is also valuable to see that you are in resize mode."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-unregistered-key-quit nil
  "Quit when an unregistered key is pressed.
If nil do not quit and notify the unregistered key pressed."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-stack-size 16
  "Size of the stack for holding window configurations."
  :type 'integer
  :group 'resize-window)

(defcustom resize-window-swap-capital-and-lowercase-behavior nil
  "Reverse default behavior of lower case and uppercase arguments."
  :type 'boolean
  :group 'resize-window)

(defcustom resize-window-notify-with-messages t
  "Show notifications in message bar."
  :type 'boolean
  :group 'resize-window)

(defvar resize-window--background-overlays ()
  "List of background overlays.")

(defvar resize-window--window-stacks ()
  "List of frame stacks for holding window configurations.

List of alists of format ((frame . ((configuration . time)...))),
time is the time when the configuration was saved/visited.")

(defvar resize-window--config-modified nil
  "Current window configuration modification flag.
It is non-nil if the configuration is new/modified.

Use `resize-window--seek-config' to initialize.")

(defvar resize-window--restore-forward t
  "Current restore movement.
It is non-nil if restoring forward, otherwise restoring backward.

Use `resize-window--seek-config' to initialize.")

(defface resize-window-background
  '((t (:foreground "gray40")))
  "Face for when resizing window.")

(defun resize-window-lowercase-argument ()
  "Return the behavior for lowercase entries.
Example, normally n maps to resize vertically by 1. However,
if you have swapped capital and lowercase behavior, then
this should return the coarse adjustment."
  (if resize-window-swap-capital-and-lowercase-behavior
      resize-window-coarse-argument
    resize-window-fine-argument))

(defun resize-window-uppercase-argument ()
  "Return the behavior for uppercase entries.
Example, normally N maps to resize vertically by 5. However,
if you have swapped capital and lowercase behavior, then this
should return the fine adjustment (default 1)."
  (if resize-window-swap-capital-and-lowercase-behavior
      resize-window-fine-argument
    resize-window-coarse-argument))

(defvar resize-window-dispatch-alist
  '((?n resize-window--resize-downward       "Resize downward" t)
    (?p resize-window--resize-upward         "Resize upward" t)
    (?f resize-window--resize-forward        "Resize forward" t)
    (?b resize-window--resize-backward       "Resize backward" t)
    (?w resize-window--cycle-window-positive "Next window" nil)
    (?W resize-window--cycle-window-negative "Previous window" nil)
    (?e resize-window--reset-windows         "Even layout (save state)" nil)
    (?2 resize-window--split-window-below    "Split below (save state)" nil)
    (?3 resize-window--split-window-right    "Split right (save state)" nil)
    (?0 resize-window--delete-window         "Delete window (save state)" nil)
    (?k resize-window--kill-other-windows    "Delete other windows (save state)" nil)
    (?x resize-window--window-drop           "Drop state" nil)
    (?s resize-window--window-save           "Save state" nil)
    (?> resize-window--restore-head          "Restore succeding (save state)" nil)
    (?< resize-window--restore-tail          "Restore preceding (save state)" nil)
    (?? resize-window--display-menu          "Toggle help menu" nil))
  "List of resize mode bindings.
Main data structure of the dispatcher with the form:
\(key function documentation allows-capitals\)")

(defvar resize-window-alias-list
  '((right ?f)
    (down ?n)
    (left ?b)
    (up ?p))
  "List of aliases for commands.
Rather than have to use n, etc, you can alias keys for others.")

(defvar resize-window--notify-timers nil
  "Notify callback timers.")

(defun resize-window--config-info ()
  "Return a string about the current window configuration.

Combines >, <, *, =, to express respectively restoring forward,
backward, new/modified and unmodified current configuration.

See also:
- `resize-window--restore-forward'
- `resize-window--config-modified'"
  (let ((a (if resize-window--restore-forward ?> ?<))
        (b (if resize-window--config-modified ?* ?=)))
    (when resize-window--restore-forward
      (let ((tmp a))
        (setq a b)
        (setq b tmp)))
    (format "%c%c" a b)))

(defun resize-window--cancel-notify ()
  "Cancel all the notify callback timers."
  (mapc 'cancel-timer resize-window--notify-timers)
  (setq resize-window--notify-timers nil))

(defun resize-window--notify (&rest info)
  "Notify with INFO, a string or list (format-string object...).
Display the status message again after a timeout.
Can be overridden in tests to test the output."
  (resize-window--cancel-notify)
  (when resize-window-notify-with-messages
    (let ((status (lambda (args)
                    (unless (minibuffer-window-active-p (selected-window))
                      (message " [%s] Resize mode: %s"
                               (resize-window--config-info)
                               (apply #'format args))))))
      (funcall status info)
      ;; FIXME: Subtle trick to update the status after side effects
      ;; have been executed (i.e. a function has run). Using a timed
      ;; callback for this is not ideal though.
      (push (run-with-timer 0.2 nil status info)
            resize-window--notify-timers)
      (push (run-with-timer 1.5 nil #'resize-window--notify-status)
            resize-window--notify-timers))))

(defun resize-window--notify-status ()
  "Display status message."
  (when resize-window-notify-with-messages
    (if (minibuffer-window-active-p (selected-window))
        (push (run-with-timer 1.5 nil #'resize-window--notify-status)
              resize-window--notify-timers)
      (message " [%s] Resize mode: insert KEY, ? for help, q or SPACE to quit"
               (resize-window--config-info)))))

(defun resize-window--key-str (key)
  "Return the string representation of KEY.
KEY is a symbol, character (integer), key text, or key sequence.

For instance, ?n \"n\" [?n] [(?n)] are considered the same, and
?\\C-n \"C-n\" \"\\C-n\" [?\\C-n] [(?\\C-n)] [(control ?n)] too."
  ;; NOTE: Fail loudly when KEY is wrong to help debugging.
  (key-description
   (cond
    ((and (not (booleanp key))
          (or (symbolp key) (integerp key)))
     (vector key))
    ((stringp key)
     (kbd key))
    ((vectorp key)
     key)
    (t
     (signal 'wrong-type-argument
             `((symbolp integerp stringp vectorp) ,key))))))

(defun resize-window--keys-equal (&rest keys)
  "Return non-nil if KEYS are considered equal.
If there is only one key return non-nil."
  (let ((key-str (resize-window--key-str (car keys))))
    (not (cl-find-if-not
          (lambda (k)
            (string= key-str (resize-window--key-str k)))
          (cdr keys)))))

(defun resize-window--key-to-lower (key)
  "Return the lowercase key sequence of KEY.
Return nil if KEY isn't an uppercase letter."
  (let* ((key-str (resize-window--key-str key))
         (char (if (= (length key-str) 1) (string-to-char key-str))))
    (and char
         (member char resize-window--capital-letters)
         (vector (+ char 32)))))

(defun resize-window--key-to-upper (key)
  "Return the uppercase key sequence of KEY.
Return nil if KEY isn't an lowercase letter."
  (let* ((key-str (resize-window--key-str key))
         (char (if (= (length key-str) 1) (string-to-char key-str))))
    (and char
         (member char resize-window--lower-letters)
         (vector (- char 32)))))

(defun resize-window--key-element (key sequence)
  "Return the first element in SEQUENCE whose car equals KEY."
  (let ((key-str (resize-window--key-str key)))
    (cl-assoc-if
     (lambda (k)
       (string= key-str (resize-window--key-str k)))
     sequence)))

(defun resize-window--match-alias (key)
  "Taken the KEY or keyboard selection check for alias.
Match the KEY against the alias table.  If found, return the value that it
points to, which should be a key in the `resize-window-dispatch-alist'.
Otherwise, return the KEY."
  (let ((alias (resize-window--key-element
                key resize-window-alias-list)))
    (if alias
        (car (cdr alias))
      key)))

(defun resize-window--match-dispatch (key)
  "Taken the KEY or keyboard selection check for an action.
Match the KEY against the alias table `resize-window-dispatch-alist'."
  (resize-window--key-element
   key resize-window-dispatch-alist))

(defun resize-window--choice-keybinding (choice)
  "Get the keybinding associated with CHOICE."
  (car choice))

(defun resize-window--choice-documentation (choice)
  "Get the documentation associated with CHOICE."
  (car (cdr (cdr choice))))

(defun resize-window--choice-lambda (choice)
  "Get the lambda associated with CHOICE."
  (car (cdr choice)))

(defun resize-window--allows-capitals (choice)
  "To save time typing, we will tell whether we allow capitals for scaling.
To do so, we check to see whether CHOICE allows for capitals by
checking its last spot in the list for whether it is true or
nil."
  (car (last choice)))

(defun resize-window--display-choice (choice)
  "Formats screen message about CHOICE.
CHOICE is a \(key function documentation allows-capitals\)."
  (let ((key (resize-window--choice-keybinding choice)))
    (concat (if (resize-window--allows-capitals choice)
                (format "%s|%s"
                        (resize-window--key-str key)
                        (resize-window--key-str
                         (resize-window--key-to-upper key)))
              (format " %s "
                      (resize-window--key-str key)))
            " : "
            (resize-window--choice-documentation choice))))

(defun resize-window--get-documentation-strings ()
  "Return documented keybindings as a multiline string."
  (mapconcat #'identity (mapcar 'resize-window--display-choice
                                resize-window-dispatch-alist)
             "\n"))

(defun resize-window--add-backgrounds ()
  "Place an overlay background over other windows."
  (resize-window--remove-backgrounds)
  (when resize-window-allow-backgrounds
    (let ((windows (remq (selected-window) (window-list nil -1))))
      (dolist (window windows)
        (with-current-buffer (window-buffer window)
          (let ((ol (make-overlay
                     (point-min)
                     (point-max)
                     (current-buffer))))
            (overlay-put ol 'face 'resize-window-background)
            (overlay-put ol 'window window)
            (push ol resize-window--background-overlays)))))))

(defun resize-window--remove-backgrounds ()
  "Remove the overlay backgrounds."
  (mapc 'delete-overlay resize-window--background-overlays)
  (setq resize-window--background-overlays nil))

(defun resize-window--execute-action (choice &optional scaled)
  "Given a CHOICE, grab values out of the alist.
CHOICE is a \(key function documentation allows-capitals\).
If SCALED, then call action with the `resize-window-uppercase-argument'."
  (let ((action (resize-window--choice-lambda choice))
        (description (resize-window--choice-documentation choice)))
    (if (resize-window--keys-equal
         (resize-window--choice-keybinding choice) [?x])
        (resize-window--cancel-notify)
      (resize-window--notify "%s" description))
    (condition-case nil
        (if scaled
            (funcall action (resize-window-uppercase-argument))
          (funcall action))
      (wrong-number-of-arguments
       (resize-window--notify
        "Invalid arity in function for %s"
        (resize-window--key-str
         (resize-window--choice-keybinding choice)))))))

;;;###autoload
(defun resize-window ()
  "Resize the window.
Press n to resize down, p to resize up, b to resize left and f
to resize right."
  (interactive)
  (resize-window--refresh-stacks)
  (resize-window--seek-config)
  ;; NOTE: Do not trim the stack here. Let stack requests to handle
  ;; window configurations in excess.
  (resize-window--add-backgrounds)
  (resize-window--notify-status)
  (condition-case nil
      (let ((reading-keys t)
            ;; allow mini-buffer to collapse after displaying menu
            (resize-mini-windows t))
        (while reading-keys
          (let* ((kin (read-key-sequence-vector nil nil t))
                 (key (and kin (resize-window--match-alias kin)))
                 (choice (and key (resize-window--match-dispatch key)))
                 (lower (and key (resize-window--key-to-lower key)))
                 (capital (and lower (resize-window--match-dispatch lower))))
            (cond
             (choice (resize-window--execute-action choice))
             ((and capital (resize-window--allows-capitals capital))
              ;; rather than pass an argument, we tell it to "scale" it
              ;; with t and that method can worry about how to get that
              ;; action
              (resize-window--execute-action capital t))
             ((or resize-window-unregistered-key-quit
                  (resize-window--keys-equal key [?q])
                  (resize-window--keys-equal key [?Q])
                  (resize-window--keys-equal key [? ])
                  (resize-window--keys-equal key "C-g"))
              (setq reading-keys nil)
              (message nil)
              (resize-window--cancel-notify)
              (resize-window--display-menu 'kill)
              (resize-window--remove-backgrounds))
             (t
              (resize-window--notify
               (format
                "Unregistered key: %s -> %s"
                key (resize-window--key-str key))))))))
    (quit
     (message nil)
     (resize-window--cancel-notify)
     (resize-window--display-menu 'kill)
     (resize-window--remove-backgrounds))))

;;; Function Handlers
(defun resize-window--resize-downward (&optional size)
  "Resize the window vertically downward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'below) 1 -1)))
      (enlarge-window (* size direction))
      (resize-window--window-modified))))

(defun resize-window--resize-upward (&optional size)
  "Resize the window vertically upward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'below) -1 1)))
      (enlarge-window (* size direction))
      (resize-window--window-modified))))

(defun resize-window--resize-forward (&optional size)
  "Resize the window horizontally forward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'right) 1 -1)))
      (enlarge-window (* size direction) t)
      (resize-window--window-modified))))

(defun resize-window--resize-backward (&optional size)
  "Resize the window horizontally backward by optional SIZE.
If no SIZE is given, modify by `resize-window-default-argument'"
  (unless (frame-root-window-p (selected-window))
    (let ((size (or size (resize-window-lowercase-argument)))
          (direction (if (window-in-direction 'right) -1 1)))
      (enlarge-window (* size direction) t)
      (resize-window--window-modified))))

(defun resize-window--reset-windows ()
  "Reset window layout to even spread."
  (when resize-window--config-modified
    (resize-window--window-save))
  (balance-windows)
  (resize-window--window-modified))

(defun resize-window--cycle-window-positive ()
  "Cycle windows."
  (other-window 1)
  (resize-window--window-modified)
  (resize-window--add-backgrounds))

(defun resize-window--cycle-window-negative ()
  "Cycle windows negative."
  (other-window -1)
  (resize-window--window-modified)
  (resize-window--add-backgrounds))

(defun resize-window--display-menu (&optional action)
  "Toggle help menu side window or perform ACTION if non-nil.
ACTION is a symbol of value 'kill or 'open."
  (let* ((buffer (get-buffer-create "*Resize-Window-Help*"))
         (window (get-buffer-window buffer))
         (add-backgrounds nil))
    (cond
     ((and window (or (not action) (eq action 'kill)))
      (quit-window t window)
      (setq add-backgrounds t))
     ((and (not window) (or (not action) (eq action 'open)))
      (setq window (display-buffer-in-side-window buffer nil))
      (set-window-parameter window 'no-other-window t)
      (set-window-parameter window 'no-delete-other-windows t)
      (with-current-buffer buffer
        (setq buffer-read-only t)
        (setq window-size-fixed t)
        (let ((inhibit-read-only t)
              (window-size-fixed nil))
          (erase-buffer)
          (insert (resize-window--get-documentation-strings))
          (fit-window-to-buffer window)))
      (setq add-backgrounds t)))
    ;; NOTE: Just in case the help menu was selected (it shouldn't)
    ;; refresh the backgrounds even when the help menu is killed.
    (when add-backgrounds
      (resize-window--add-backgrounds))))

(defun resize-window--split-window-below ()
  "Split the window vertically."
  (when resize-window--config-modified
    (resize-window--window-save))
  (split-window-below)
  (resize-window--window-modified)
  (resize-window--add-backgrounds))

(defun resize-window--split-window-right ()
  "Split the window horizontally."
  (when resize-window--config-modified
    (resize-window--window-save))
  (split-window-right)
  (resize-window--window-modified)
  (resize-window--add-backgrounds))

(defun resize-window--delete-window ()
  "Delete the current window."
  (unless (eq (selected-window) (window-main-window))
    (when resize-window--config-modified
      (resize-window--window-save))
    (delete-window)
    (resize-window--window-modified)
    (resize-window--add-backgrounds)))

(defun resize-window--window-config ()
  "Return the current window configuration.
Exclude the help menu from the configuration."
  (let ((display-menu (get-buffer-window "*Resize-Window-Help*")))
    (resize-window--display-menu 'kill)
    (prog2
        ;; WORKAROUND: Calling `current-buffer' or `get-buffer-window'
        ;; soon after `ivy-switch-buffer' references the old buffer.
        ;; This forces to update to the buffer switched to.  It also
        ;; allows `current-window-configuration' to capture a proper
        ;; configuration updating the values of all current buffers.
        ;; See also https://github.com/abo-abo/swiper/issues/1766
        (let ((curr-window (selected-window)))
          (mapc (lambda (w) (select-window w)) (window-list))
          (select-window curr-window))
        (current-window-configuration)
      (when display-menu
        (resize-window--display-menu 'open)))))

(defun resize-window--restore-config (config)
  "Restore the window configuration CONFIG then return it.
Restore the help menu only if it is currently open."
  (let ((display-menu (get-buffer-window "*Resize-Window-Help*")))
    (set-window-configuration config)
    ;; NOTE: If `resize-window--window-config' was used to save the
    ;; CONFIG there is no help menu to kill. Keep this just in case.
    (resize-window--display-menu 'kill)
    (prog1
        (current-window-configuration)
      (if display-menu
          (resize-window--display-menu 'open)
        (resize-window--add-backgrounds)))))

(defun resize-window--apply-config (config)
  "Return the window configuration CONFIG after applying it.
Return nil if CONFIG isn't a proper window configuration.
Do not change the current window configuration."
  (when (window-configuration-p config)
    (let ((curr-frame (selected-frame))
          (some-frame (window-configuration-frame config))
          (some-config config))
      (when (frame-live-p some-frame)
        (select-frame some-frame)
        (save-excursion
          (save-window-excursion
            (set-window-configuration config)
            (setq some-config (resize-window--window-config))))
        (select-frame curr-frame))
      some-config)))

(defun resize-window--seek-config (&optional config frame)
  "Seek the most recent version of CONFIG in FRAME's stack.
If CONFIG is nil use the current window configuration.
If FRAME is nil use the current frame.
If CONFIG is found return its stack member, otherwise return nil.

If CONFIG is found, unset the `resize-window--config-modified'
flag and reorganize the stack with CONFIG as its first element
and all the elements before CONFIG as part of its tail.

If CONFIG isn't found, set `resize-window--config-modified'.

See also `resize-window--refresh-stacks'."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (curr-config (or config (resize-window--window-config)))
         (curr-member nil)
         (curr-svtime 0)
         (head nil)
         (tail nil))
    (dolist (this-member stack)
      (let ((this-config (car this-member))
            (this-svtime (cdr this-member)))
        (when (and (compare-window-configurations curr-config this-config)
                   (time-less-p curr-svtime this-svtime))
          (setq curr-svtime this-svtime)
          (setq curr-member this-member)
          (setq tail (append tail head))
          (setq head nil))
        (setq head (append head (list this-member)))))
    (when curr-member
      (setq curr-member (cons curr-config (current-time)))
      (setq stack (append head tail))
      (setcar stack curr-member)
      (setcdr frame-stack stack))
    (setq resize-window--config-modified (not curr-member))
    curr-member))

(defun resize-window--refresh-stacks ()
  "Refresh the stack for all the frames.

See also `resize-window--refresh-frame-stack'."
  (dolist (frame-stack resize-window--window-stacks)
    (let ((frame (car frame-stack)))
      (resize-window--refresh-frame-stack frame))))

(defun resize-window--refresh-frame-stack (&optional frame)
  "Refresh FRAME's stack and remove adjacent duplicates.
Each window configuration is restored and saved again.

The configurations saved time is not changed. Always remove the
older configuration when a duplicate is found.

A refresh reveals duplicate configurations. When a configuration
is restored that takes account of the current state of the frame.
Since killed buffers cannot be dug up, applying a state will use
what it finds, and so two configurations may end up the same."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (stack-buffer nil))
    (dotimes (n (length stack))
      (let* ((this-member (nth n stack))
             (this-config (resize-window--apply-config (car this-member)))
             (this-svtime (cdr this-member))
             (prev-config (caar stack-buffer))
             (prev-svtime (cdar stack-buffer)))
        (if (and this-config prev-config
                 (compare-window-configurations this-config prev-config))
            (when (time-less-p prev-svtime this-svtime)
              (setcar stack-buffer this-member))
          (when this-config
            (push this-member stack-buffer)))))
    (setq stack (nreverse stack-buffer))
    (setcdr frame-stack stack)
    (unless stack
      (resize-window--del-frame-stack frame))))

(defun resize-window--frame-stack (&optional frame)
  "Return the FRAME's window configurations stack.
If FRAME is nil use the current frame."
  (when (setq frame (or frame (selected-frame)))
    (assq frame resize-window--window-stacks)))

(defun resize-window--del-frame-stack (&optional frame)
  "Remove FRAME's window configurations stack.
If FRAME is nil use the current frame.
Return the removed FRAME's stack."
  (let ((frame-stack (resize-window--frame-stack frame)))
    (when frame-stack
      (setq resize-window--window-stacks
            (delq frame-stack resize-window--window-stacks))
      frame-stack)))

(defun resize-window--get-stack-head (&optional frame)
  "Return the first member in FRAME's window configurations stack.
If FRAME is nil use the current frame."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack)))
    (car stack)))

(defun resize-window--pop-stack-head (&optional frame)
  "Remove the first member from FRAME's window configurations stack.
If FRAME is nil use the current frame.
Return the removed member."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (this-member (car stack))
         (rest-members (cdr stack)))
    (when (consp frame-stack)
      (setcdr frame-stack rest-members)
      (unless rest-members
        (resize-window--del-frame-stack frame))
      this-member)))

(defun resize-window--push-stack-head (element &optional frame)
  "Push ELEMENT to the begin of FRAME's window configurations stack.
If FRAME is nil use the current frame.
Return ELEMENT."
  (when (and element (setq frame (or frame (selected-frame))))
    (let* ((frame-stack
            (or (resize-window--frame-stack frame)
                (car (push (list frame)
                           resize-window--window-stacks))))
           (stack (cdr frame-stack)))
      (car (setcdr frame-stack
                   (append (list element) stack))))))

(defun resize-window--set-stack-head (element &optional frame)
  "Replace the first member in FAME's window configurations stack
with ELEMENT then return ELEMENT.
Push ELEMENT in the stack if the stack is empty.
If FRAME is nil use the current frame."
  (when (and element (setq frame (or frame (selected-frame))))
    (let* ((frame-stack
            (or (resize-window--frame-stack frame)
                (car (push (list frame)
                           resize-window--window-stacks))))
           (stack (cdr frame-stack)))
      (if (consp stack)
          (setcar stack element)
        (car (setcdr frame-stack (list element)))))))

(defun resize-window--stack-head-config (config &optional frame)
  "Replace the first configuration in FRAME's window configurations
stack with CONFIG then return CONFIG.
If FRAME is nil use the current frame."
  (when config
    (let ((element (resize-window--get-stack-head frame)))
      (when element
        (setcar element config)))))

(defun resize-window--stack-head-svtime (save-time &optional frame)
  "Replace the first configuration's save time in FRAME's window
configurations stack to SAVE-TIME then return SAVE-TIME.
If FRAME is nil use the current frame."
  (when save-time
    (let ((element (resize-window--get-stack-head frame)))
      (when element
        (setcdr element save-time)))))

(defun resize-window--get-stack-tail (&optional frame)
  "Return the last member in FRAME's window configurations stack.
If FRAME is nil use the current frame."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack)))
    (car (last stack))))

(defun resize-window--pop-stack-tail (&optional frame)
  "Remove the last member from FRAME's window configurations stack.
If FRAME is nil use the current frame.
Return the removed member."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (this-member (car (last stack)))
         (rest-members (nbutlast stack)))
    (when (consp frame-stack)
      (setcdr frame-stack rest-members)
      (unless rest-members
        (resize-window--del-frame-stack frame))
      this-member)))

(defun resize-window--push-stack-tail (element &optional frame)
  "Push ELEMENT to the end of FRAME's window configurations stack.
If FRAME is nil use the current frame.
Return ELEMENT."
  (when (and element (setq frame (or frame (selected-frame))))
    (let* ((frame-stack
            (or (resize-window--frame-stack frame)
                (car (push (list frame)
                           resize-window--window-stacks))))
           (stack (cdr frame-stack)))
      (car (last (setcdr frame-stack
                         (append stack (list element))))))))

(defun resize-window--set-stack-tail (element &optional frame)
  "Replace the last member in FRAME's window configurations stack
with ELEMENT then return ELEMENT.
Push ELEMENT in the stack if the stack is empty.
If FRAME is nil use the current frame."
  (when (and element (setq frame (or frame (selected-frame))))
    (let* ((frame-stack
            (or (resize-window--frame-stack frame)
                (car (push (list frame)
                           resize-window--window-stacks))))
           (stack (cdr frame-stack)))
      (if (consp stack)
          (setcar (last stack) element)
        (car (setcdr frame-stack (list element)))))))

(defun resize-window--stack-tail-config (config &optional frame)
  "Replace the last configuration in FRAME's window configurations
stack with CONFIG then return CONFIG.
If FRAME is nil use the current frame."
  (when config
    (let ((element (resize-window--get-stack-tail frame)))
      (when element
        (setcar element config)))))

(defun resize-window--stack-tail-svtime (save-time &optional frame)
  "Replace the last configuration's save time in FRAME's window
configurations stack with SAVE-TIME then return SAVE-TIME.
If FRAME is nil use the current frame."
  (when save-time
    (let ((element (resize-window--get-stack-tail frame)))
      (when element
        (setcdr element save-time)))))

(defun resize-window--get-stack-member (&optional from-tail frame)
  "Return the first member in FRAME's window configurations stack.
If FROM-TAIL is non-nil return the last member intead.
If FRAME is nil use the current frame."
  (if from-tail
      (resize-window--get-stack-tail frame)
    (resize-window--get-stack-head frame)))

(defun resize-window--pop-stack-member (&optional from-tail frame)
  "Remove the first member from FRAME's window configurations stack.
If FROM-TAIL is non-nil remove the last member instead.
If FRAME is nil use the current frame.
Return the removed member."
  (if from-tail
      (resize-window--pop-stack-tail frame)
    (resize-window--pop-stack-head frame)))

(defun resize-window--push-stack-member (element &optional to-tail frame)
  "Push ELEMENT to the begin of FRAME's window configurations stack.
If TO-TAIL is non-nil push to the end.
If FRAME is nil use the current frame.
Return ELEMENT."
  (if to-tail
      (resize-window--push-stack-tail element frame)
    (resize-window--push-stack-head element frame)))

(defun resize-window--set-stack-member (element &optional to-tail frame)
  "Replace the first member in FRAME's window configurations stack
with ELEMENT then return ELEMENT.
If TO-TAIL is non-nil replace the last.
If FRAME is nil use the current frame."
  (if to-tail
      (resize-window--set-stack-tail element frame)
    (resize-window--set-stack-head element frame)))

(defun resize-window--stack-member-config (config &optional to-tail frame)
  "Replace the first configuration in FRAME's window configurations
stack with CONFIG then return CONFIG.
If TO-TAIL is non-nil replace the last.
If FRAME is nil use the current frame."
  (if to-tail
      (resize-window--stack-tail-config config frame)
    (resize-window--stack-head-config config frame)))

(defun resize-window--stack-member-svtime (save-time &optional to-tail frame)
  "Replace the first configuration's save time in FRAME's window
configurations stack with SAVE-TIME then return SAVE-TIME.
If TO-TAIL is non-nil replace the last.
If FRAME is nil use the current frame."
  (if to-tail
      (resize-window--stack-tail-svtime save-time frame)
    (resize-window--stack-head-svtime save-time frame)))

(defun resize-window--get-stack-nth (n &optional frame)
  "Return the Nth member in FRAME's window configurations stack.
If N is negative count from the end, where -1 is the end.
If FRAME is nil use the current frame.
Return nil if N is out of bound."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (stack-size (length stack))
         (stack-last (1- stack-size))
         (x (if (< n 0) (+ stack-size n) n)))
    (and (>= x 0)
         (<= x stack-last)
         (nth x stack))))

(defun resize-window--pop-stack-nth (n &optional frame)
  "Remove the Nth member from FRAME's window configurations stack.
If N is negative count from the end, where -1 is the end.
Return the removed member or nil if N is out of bound.
If FRAME is nil use the current frame."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (stack-size (length stack))
         (stack-last (1- stack-size))
         (x (if (< n 0) (+ stack-size n) n)))
    (and (>= x 0)
         (<= x stack-last)
         (prog1
             (pop (nthcdr x stack))
           (setcdr frame-stack stack)
           (unless stack
             (resize-window--del-frame-stack frame))))))

(defun resize-window--push-stack-nth (element n &optional frame)
  "Push ELEMENT as the Nth member in FRAME's window configurations
stack then return ELEMENT or nil if N is out of bound.
If N is negative count from the end, where -1 is the end.
If FRAME is nil use the current frame."
  (setq frame (or frame (selected-frame)))
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (stack-size (length stack))
         (stack-last (1- stack-size))
         (x (if (< n 0) (+ stack-size n 1) n)))
    (when (and element (>= x 0) (<= x stack-size))
      (let ((head (butlast stack (- stack-size x)))
            (tail (nthcdr x stack)))
        (setq stack (append head (list element) tail))
        (unless frame-stack
          (setq frame-stack
                (car (push (list frame)
                           resize-window--window-stacks))))
        (setcdr frame-stack stack)
        element))))

(defun resize-window--set-stack-nth (element n &optional frame)
  "Replace the Nth member in FRAME's window configurations stack
with ELEMENT then return ELEMENT or nil if N is out of bound.
Push ELEMENT in the stack if the stack is empty with N 0 or -1.
If N is negative count from the end, where -1 is the end.
If FRAME is nil use the current frame."
  (setq frame (or frame (selected-frame)))
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (stack-size (length stack))
         (stack-last (if (> stack-size 0) (1- stack-size) 0))
         (x (if (< n 0) (+ stack-size n (if (> stack-size 0) 0 1)) n)))
    (when (and element (>= x 0) (<= x stack-last))
      (let ((head (butlast stack (- stack-size x)))
            (tail (nthcdr (1+ x) stack)))
        (setq stack (append head (list element) tail))
        (unless frame-stack
          (setq frame-stack
                (car (push (list frame)
                           resize-window--window-stacks))))
        (setcdr frame-stack stack)
        element))))

(defun resize-window--stack-nth-config (config n &optional frame)
  "Replace the Nth configuration in FRAME's window configurations
stack with CONFIG then return CONFIG or nil if N is out of bound.
If N is negative count from the end, where -1 is the end.
If FRAME is nil use the current frame."
  (when config
    (let ((element (resize-window--get-stack-nth n frame)))
      (when element
        (setcar element config)))))

(defun resize-window--stack-nth-svtime (save-time n &optional frame)
  "Replace the Nth configuration's save time in FRAME's window
configurations stack with SAVE-TIME then return SAVE-TIME or nil
if N is out of bound.
If N is negative count from the end, where -1 is the end.
If FRAME is nil use the current frame."
  (when save-time
    (let ((element (resize-window--get-stack-nth n frame)))
      (when element
        (setcdr element save-time)))))

(defun resize-window--stack-shift-head (&optional frame)
  "Shift right in FRAME's stack.
Return the member shifted to if any.
If FRAME is nil use the current frame.

Pop the head and push it to the tail in FRAME's stack. It is like
scrolling right in the stack. The next member becomes the first."
  (let ((element (resize-window--pop-stack-head frame)))
    (when element
      (resize-window--push-stack-tail element frame))))

(defun resize-window--stack-shift-tail (&optional frame)
  "Shift left in FRAME's stack.
Return the member shifted to if any.
If FRAME is nil use the current frame.

Pop the tail and push it to the head in FRAME's stack. It is like
scrolling left in the stack. The last member becomes the first."
  (let ((element (resize-window--pop-stack-tail frame)))
    (when element
      (resize-window--push-stack-head element frame))))

(defun resize-window--stack-shift-member (&optional to-tail frame)
  "Shitf right in FRAME's stack.
If TO-TAIL is non-nil shift left.
Return the member shifted to if any.

Move the head to the tail like scrolling right, or move the tail
to the head like scrolling left if TO-TAIL is non-nil."
  (if to-tail
      (resize-window--stack-shift-tail frame)
    (resize-window--stack-shift-head frame)))

(defun resize-window--stack-config-modified (&optional config from-tail frame)
  "Return non-nil if CONFIG differs from FRAME's stack first configuration.
If CONFIG is nil use the current window configuration.
If FROM-TAIL is non-nil check the last configuration.
If FRAME is nil use the current frame."
  (let* ((curr-config (or config (resize-window--window-config)))
         (this-member (if from-tail
                      (resize-window--get-stack-tail frame)
                    (resize-window--get-stack-head frame)))
         (this-config (car this-member)))
    (or (not (setq this-config (resize-window--apply-config this-config)))
        (not (compare-window-configurations curr-config this-config)))))

(defun resize-window--trim-stack-dups (&optional config from-tail frame)
  "Trim consecutive duplicates of CONFIG from the beginning of
FRAME's stack or from the end if FROM-TAIL is non-nil.
If CONFIG is nil use the current window configuration.
If FRAME is nil use the current frame.
Return the first stack member that differs from CONFIG."
  (let ((curr-config (or config (resize-window--window-config)))
        (this-member nil)
        (this-config nil))
    (while
        (and (setq this-member
                   (resize-window--get-stack-member from-tail frame))
             (setq this-config (car this-member))
             (setq this-config (resize-window--apply-config this-config))
             (compare-window-configurations curr-config this-config)
             (resize-window--pop-stack-member from-tail frame)))
    this-member))

(defun resize-window--window-trim (&optional frame stack-size)
  "Trim the oldest window configurations from FRAME's stack in
excess of STACK-SIZE then return the removed stack members.
If FRAME is nil use the current frame.
If STACK-SIZE is nil use `resize-window-stack-size'."
  (let* ((frame-stack (resize-window--frame-stack frame))
         (stack (cdr frame-stack))
         (size (length stack))
         (trim (- size (or stack-size resize-window-stack-size))))
    (when (> trim 0)
      (let ((oldest-members
             (sort (copy-sequence stack)
                   (lambda (a b)
                     (time-less-p (cdr a) (cdr b))))))
        (setq oldest-members
              (nbutlast oldest-members (- size trim)))
        (dotimes (n (length oldest-members))
          (let ((old-member (nth n oldest-members)))
            (setq stack (delq old-member stack))))
        (setcdr frame-stack stack)
        (unless stack
          (resize-window--del-frame-stack frame))
        oldest-members))))

(defun resize-window--window-drop ()
  "Drop the current window configuration from the stack.
Ask the user for confirmation then return the removed member.

Abort if the configuration isn't in the stack or the user decided
otherwise. If the configuration is dropped, switch to another one
in respect to `resize-window--restore-forward' direction flag."
  (if (or (resize-window--stack-config-modified)
          (not (let ((query-replace-map (copy-keymap query-replace-map)))
                 (define-key query-replace-map [? ] 'skip)
                 (y-or-n-p "Drop saved state? "))))
      (resize-window--notify-status)
    (prog1
        (resize-window--pop-stack-member)
      (unless resize-window--restore-forward
        (resize-window--stack-shift-member t))
      (let* ((curr-member (resize-window--get-stack-member))
             (curr-config (car curr-member)))
        (when curr-config
          (resize-window--restore-config curr-config)
          (setq curr-config (resize-window--window-config))
          (resize-window--stack-member-config curr-config)
          (resize-window--stack-member-svtime (current-time))))
      (setq resize-window--config-modified
            (resize-window--stack-config-modified))
      (resize-window--notify "Drop saved state"))))

(defun resize-window--window-save ()
  "Save the current window configuration in the stack.
If the configuration is saved return its stack member.

If the configuration isn't modified, replace the previously saved
configuration, otherwise save the configuration in respect to the
`resize-window--restore-forward' flag, either after or before the
first element of the stack.

Set `resize-window--config-modified' to the configuration state.

Trim adjacent duplicates and old configurations when necessary to
fit `resize-window-stack-size'."
  (let* ((curr-config (resize-window--window-config))
         (curr-member (cons curr-config (current-time)))
         (head-change (resize-window--stack-config-modified curr-config)))
    (when (and head-change resize-window--restore-forward)
      (let* ((this-member (resize-window--pop-stack-member))
             (this-config (car this-member)))
        (resize-window--trim-stack-dups this-config)
        (resize-window--trim-stack-dups this-config t)
        (resize-window--push-stack-member this-member t)))
    (resize-window--trim-stack-dups curr-config)
    (resize-window--trim-stack-dups curr-config t)
    (resize-window--push-stack-member curr-member)
    (resize-window--window-trim)
    (setq resize-window--config-modified
          (resize-window--stack-config-modified))
    (when (eq curr-member (resize-window--get-stack-member))
      curr-member)))

(defun resize-window--window-shift (&optional to-tail)
  "Shift to the next window configuration in the stack.
If TO-TAIL is non-nil shift to the left, otherwise to the right.
Return the stack member of the configuration shifted to if any.

If the current configuration is modified, save the configuration
via `resize-window--window-save' before shifting.

Set `resize-window--restore-forward' accordingly to the shift and
set `resize-window--config-modified' to the configuration state.

Trim adjacent duplicates and old configurations when necessary to
fit `resize-window-stack-size'."
  (when (resize-window--stack-config-modified)
    (resize-window--window-save))
  (let* ((curr-member (resize-window--get-stack-member))
         (this-member (resize-window--pop-stack-member to-tail))
         (this-config (resize-window--apply-config (car this-member)))
         (head-member (resize-window--trim-stack-dups this-config))
         (tail-member (resize-window--trim-stack-dups this-config t))
         (next-member (if to-tail
                          this-member
                        head-member))
         (next-config (if to-tail
                          this-config
                        (resize-window--apply-config (car next-member)))))
    (resize-window--push-stack-member this-member (not to-tail))
    (if (eq curr-member next-member)
        (setq next-member nil)
      (resize-window--stack-member-config next-config)
      (resize-window--stack-member-svtime (current-time)))
    (resize-window--window-trim)
    (setq resize-window--restore-forward (not to-tail))
    (setq resize-window--config-modified
          (resize-window--stack-config-modified))
    next-member))

(defun resize-window--window-modified ()
  "Seek the adjacent window configuration equal to the current.
Return the matched stack member if found, otherwise return nil.

Configurations adjacent to the current are the first, next, or
last in the stack.

If a match is found unset `resize-window--config-modified' and
reorganize the stack with the match as its first element. If a
match isn't found set `resize-window--config-modified'.

Also, if a match is found set `resize-window--restore-forward'
accordingly in respect to where the match was found, before or
after the first configuration in the stack."
  (let* ((curr-config (resize-window--window-config))
         (curr-member (cons curr-config (current-time)))
         (head-member (resize-window--get-stack-nth 0))
         (next-member (resize-window--get-stack-nth 1))
         (tail-member (resize-window--get-stack-nth -1))
         (find-config
          (lambda (this-member)
            (let ((this-config (car this-member)))
              (when this-config
                (setq this-config (resize-window--apply-config this-config))
                (compare-window-configurations curr-config this-config)))))
         head-equals next-equals tail-equals)
    (when (eq next-member tail-member)
      (if resize-window--restore-forward
          (setq tail-member nil)
        (setq next-member nil)))
    (if resize-window--restore-forward
        (or (setq head-equals (funcall find-config head-member))
            (setq next-equals (funcall find-config next-member))
            (setq tail-equals (funcall find-config tail-member)))
      (or (setq head-equals (funcall find-config head-member))
          (setq tail-equals (funcall find-config tail-member))
          (setq next-equals (funcall find-config next-member))))
    (setq resize-window--config-modified
          (not (or head-equals next-equals tail-equals)))
    (unless resize-window--config-modified
      (unless head-equals
        (setq resize-window--restore-forward next-equals)
        (resize-window--stack-shift-member (not next-equals)))
      (resize-window--set-stack-member curr-member))))

(defun resize-window--kill-other-windows ()
  "Delete other windows."
  (when resize-window--config-modified
    (resize-window--window-save))
  (delete-other-windows)
  (resize-window--window-modified)
  (resize-window--add-backgrounds))

(defun resize-window--restore-head ()
  "Restore a succeding state.
Set `resize-window--config-modified' to the configuration state."
  (let* ((prev-member (resize-window--window-shift))
         (prev-config (car prev-member)))
    (when prev-config
      (resize-window--restore-config prev-config)
      (setq resize-window--config-modified
            (resize-window--stack-config-modified)))))

(defun resize-window--restore-tail ()
  "Restore a preceding state.
Set `resize-window--config-modified' to the configuration state."
  (let* ((prev-member (resize-window--window-shift t))
         (prev-config (car prev-member)))
    (when prev-config
      (resize-window--restore-config prev-config)
      (setq resize-window--config-modified
            (resize-window--stack-config-modified)))))

(defvar resize-window--capital-letters (number-sequence ?A ?Z)
  "List of uppercase letters as characters.")
(defvar resize-window--lower-letters (number-sequence ?a ?z)
  "List of lowercase letters as characters.")

(defun resize-window--key-available? (key)
  "Return non-nil if KEY is bound, otherwise return nil."
  (and (not (resize-window--key-element
             key resize-window-alias-list))
       (not (resize-window--key-element
             key resize-window-dispatch-alist))))

(defun resize-window-add-choice (key func doc &optional allows-capitals force)
  "Register a new binding for `resize-window'.
Refuses to replace an already taken key unless FORCE is non-nil.

KEY is the key (e.g. ?c) that invokes the function FUNC. DOC is a
docstring for the help menu. A non-nil ALLOWS-CAPITALS tells FUNC
accepts capital letters. FUNC should be of zero arity if does not
allow capitals, otherwise to allow capitals should be of optional
single arity so a capital KEY may be passed to FUNC when pressed.

See also `resize-window--key-str'."
  (when force
    (setq resize-window-alias-list
          (delq (resize-window--key-element
                 key resize-window-alias-list)
                resize-window-alias-list))
    (setq resize-window-dispatch-alist
          (delq (resize-window--key-element
                 key resize-window-dispatch-alist)
                resize-window-dispatch-alist)))
  (if (resize-window--key-available? key)
      (push (list key func doc allows-capitals)
            resize-window-dispatch-alist)
    (message "The `%s` key is already taken for resize-window."
             (resize-window--key-str key))))

(provide 'resize-window)
;;; resize-window.el ends here
