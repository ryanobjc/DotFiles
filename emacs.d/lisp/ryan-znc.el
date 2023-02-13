(require 'erc)

(defcustom ryan-erc-insert-away-timestamp-function
  #'ryan-erc-insert-timestamp-left-and-right
  "Function to use to insert the away timestamp.

See `erc-insert-timestamp-function' for details."
  :type '(choice (const :tag "Both sides" ryan-erc-insert-timestamp-left-and-right)
		 (const :tag "Right" ryan-erc-insert-timestamp-right)
		 (const :tag "Left" ryan-erc-insert-timestamp-left)
		 function))

(defcustom ryan-erc-timestamp-format "[%H:%M]"
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%f\" and \"%H:%M\".

If nil, timestamping is turned off."
  :type '(choice (const nil)
                 (string)))

(defcustom ryan-erc-timestamp-format-left "\n[%a %b %e %Y]\n"
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

This timestamp is used for timestamps on the left side of the
screen when `erc-insert-timestamp-function' is set to
`erc-insert-timestamp-left-and-right'.

If nil, timestamping is turned off."
  :type '(choice (const nil)
		 (string)))

(defcustom ryan-erc-timestamp-format-right " [%H:%M]"
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

This timestamp is used for timestamps on the right side of the
screen when `erc-insert-timestamp-function' is set to
`erc-insert-timestamp-left-and-right'.

If nil, timestamping is turned off."
  :type '(choice (const nil)
		 (string)))


(defcustom ryan-erc-insert-timestamp-function 'ryan-erc-insert-timestamp-left-and-right
  "Function to use to insert timestamps.

It takes a single argument STRING which is the final string
which all text-properties already appended.  This function only cares about
inserting this string at the right position.  Narrowing is in effect
while it is called, so (point-min) and (point-max) determine the region to
operate on.

You will probably want to set
`erc-insert-away-timestamp-function' to the same value."
  :type '(choice (const :tag "Both sides" ryan-erc-insert-timestamp-left-and-right)
		 (const :tag "Right" ryan-erc-insert-timestamp-right)
		 (const :tag "Left" ryan-erc-insert-timestamp-left)
		 function))

(defvar ryan-erc-last-server-time-seen nil
  )

(defvar-local ryan-erc-timestamp-last-window-width nil
  "The width of the last window that showed the current buffer.
his is used by `erc-insert-timestamp-right' when the current
buffer is not shown in any window.")

(defvar-local ryan-erc-timestamp-last-inserted nil
  "Last timestamp inserted into the buffer.")

(defvar-local ryan-erc-timestamp-last-inserted-left nil
  "Last timestamp inserted into the left side of the buffer.
This is used when `erc-insert-timestamp-function' is set to
`erc-timestamp-left-and-right'")

(defvar-local ryan-erc-timestamp-last-inserted-right nil
  "Last timestamp inserted into the right side of the buffer.
This is used when `erc-insert-timestamp-function' is set to
`erc-timestamp-left-and-right'")


(define-erc-module ryan-stamp rytimestamp
  "This mode timestamps messages in the channel buffers."
  ((add-hook 'erc-mode-hook #'erc-munge-invisibility-spec)
   ;;(advice-add 'erc-login :before 'ryan-server-time-enable)
   ;;(add-hook 'erc-server-001-functions #'ryan-server-time-enable)
   (add-hook 'erc-join-hook #'ryan-request-chan-playback)
   (add-hook 'erc-insert-modify-hook #'ryan-erc-add-timestamp t)
   (add-hook 'erc-send-modify-hook #'ryan-erc-add-timestamp t))
  ((remove-hook 'erc-mode-hook #'erc-munge-invisibility-spec)
   ;;(advice-remove 'erc-login 'ryan-server-time-enable)
   ;;(remove-hook 'erc-server-001-functions #'ryan-server-time-enable)
   (remove-hook 'erc-join-hook #'ryan-request-chan-playback)
   (remove-hook 'erc-insert-modify-hook #'ryan-erc-add-timestamp)
   (remove-hook 'erc-send-modify-hook #'ryan-erc-add-timestamp)))

(cl-defmethod erc--register-connection :before ()
  (ryan-server-time-enable))

(defun ryan-server-time-enable ()
  (erc-server-send "CAP REQ :server-time" t)
  (erc-server-send "CAP REQ znc.in/playback" t)
  ;;(erc-server-send "CAP REQ :echo-message")
  )


(defun ryan-request-chan-playback ()
  ;; We are in the buffer, so what was the most previous line?
  ;; move to the end first.
  (message "Potentially requesting channel playback!")
  (save-excursion
    ;; the point might not be at the end of the buffer,
    ;; we could be requesting too much playback
    (goto-char (point-max))
    (goto-char (pos-bol))

    ;; also consider stepping back until we find a message which has the
    ;; text property of 'font-lock-face of 'erc-default-face
    (cl-loop
     until (or (eq (get-text-property (point) 'font-lock-face) 'erc-default-face)
               (eq (point) (point-min)))
     do (forward-line -1))
    (message "Playback for %s: The line we've settled on is `%s'"
             (buffer-name)
             (or (buffer-substring (pos-bol) (pos-eol)) ""))
     
    ;;(let* ((prev-msg (1- (pos-bol)))
    (let ((prev-ts (get-text-property (point) 'erc-ts)))
      (when prev-ts
        (message (format "Requesting playback from last message (char %d) time %d on %s"
                         (point) prev-ts (buffer-name)))
        (erc-server-send (format "PRIVMSG *playback :play %s %d" (buffer-name) prev-ts))))))

;;; store the time for later playback
;; dont store the time if we have just connected and haven't played back?
;; store the data per channel?
(defun ryan-maybe-get-time ()
  "Get the timestamp from the server tags properties on the string"
  (let ((server-ts (cadr (assoc "time" (get-text-property (point) 'tags)))))
    (if server-ts
        (let ((ts (encode-time (parse-time-string server-ts))))
          (setq ryan-erc-last-server-time-seen (time-convert ts 'integer))
          ts)
      (progn
        ;(message "local timestamp! :-(")
        (current-time)))))


(defun ryan-erc-add-timestamp ()
  "Add timestamp and text-properties to message.

This function is meant to be called from `erc-insert-modify-hook'
or `erc-send-modify-hook'."
  ;(message "ryan erc add timestamp!!")
  (unless (get-text-property (point) 'invisible)
    (let* ((ct (ryan-maybe-get-time))
           (ts (time-convert ct 'integer)))
      (if (fboundp ryan-erc-insert-timestamp-function)
	  (funcall ryan-erc-insert-timestamp-function
		   (erc-format-timestamp ct ryan-erc-timestamp-format))
	(error "Timestamp function unbound"))
      (when (and (fboundp erc-insert-away-timestamp-function)
		 erc-away-timestamp-format
		 (erc-away-time)
		 (not ryan-erc-timestamp-format))
	(funcall erc-insert-away-timestamp-function
		 (erc-format-timestamp ct erc-away-timestamp-format)))
                           
      (add-text-properties (point-min) (point-max)
			   ;; It's important for the function to
			   ;; be different on different entries (bug#22700).
			   (list 'cursor-sensor-functions
				 (list (lambda (_window _before dir)
					 (erc-echo-timestamp dir ct)))
                                 'erc-ts
                                 ts
                                 )))))



(defun ryan-erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
			(string-equal string erc-timestamp-last-inserted)))
	 (len (length string))
	 (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (erc-put-text-property 0 len 'invisible 'timestamp s)
    (insert s)))

(defun ryan-erc-insert-timestamp-right (string)
  "Insert timestamp on the right side of the screen.
STRING is the timestamp to insert.  This function is a possible
value for `erc-insert-timestamp-function'.

If `erc-timestamp-only-if-changed-flag' is nil, a timestamp is
always printed.  If this variable is non-nil, a timestamp is only
printed if it is different from the last.

If `erc-timestamp-right-column' is set, its value will be used as
the column at which the timestamp is to be printed.  If it is
nil, and `erc-fill-mode' is active, then the timestamp will be
printed just before `erc-fill-column'.  Otherwise, if the current
buffer is shown in a window, that window's width is used as the
right boundary.  In case multiple windows show the buffer, the
width of the most recently selected one is used.  If the buffer
is not shown, the timestamp will be printed just before the
window width of the last window that showed it.  If the buffer
was never shown, and `fill-column' is set, it will be printed
just before `fill-column'.  As a last resort, timestamp will be
printed just after each line's text (no alignment)."
  (unless (and erc-timestamp-only-if-changed-flag
	       (string-equal string ryan-erc-timestamp-last-inserted))
    (setq ryan-erc-timestamp-last-inserted string)
    (goto-char (point-max))
    (forward-char -1)                   ; before the last newline
    (let* ((str-width (string-width string))
           window                  ; used in computation of `pos' only
	   (pos (cond
		 (erc-timestamp-right-column erc-timestamp-right-column)
		 ((and (boundp 'erc-fill-mode)
		       erc-fill-mode
		       (boundp 'erc-fill-column)
		       erc-fill-column)
		  (1+ (- erc-fill-column str-width)))
                 ((setq window (get-buffer-window nil t))
                  (setq erc-timestamp-last-window-width
                        (window-width window))
                  (- erc-timestamp-last-window-width str-width))
                 (erc-timestamp-last-window-width
                  (- erc-timestamp-last-window-width str-width))
		 (fill-column
		  (1+ (- fill-column str-width)))
                 (t (current-column))))
	   (from (point))
	   (col (current-column)))
      ;; The following is a kludge used to calculate whether to move
      ;; to the next line before inserting a stamp.  It allows for
      ;; some margin of error if what is displayed on the line differs
      ;; from the number of characters on the line.
      (setq col (+ col (ceiling (/ (- col (- (point) (line-beginning-position))) 1.6))))
      (if (< col pos)
	  (erc-insert-aligned string pos)
	(newline)
	(indent-to pos)
	(setq from (point))
	(insert string))
      (erc-put-text-property from (point) 'field 'erc-timestamp)
      (erc-put-text-property from (point) 'rear-nonsticky t)
      (when erc-timestamp-intangible
	(erc-put-text-property from (1+ (point)) 'cursor-intangible t)))))


(defun ryan-erc-insert-timestamp-left-and-right (_string)
  "This is another function that can be used with `erc-insert-timestamp-function'.
If the date is changed, it will print a blank line, the date, and
another blank line.  If the time is changed, it will then print
it off to the right."
  (let* ((ct (ryan-maybe-get-time))
	 (ts-left (erc-format-timestamp ct ryan-erc-timestamp-format-left))
	 (ts-right (erc-format-timestamp ct ryan-erc-timestamp-format-right)))
    ;; insert left timestamp
    (unless (string-equal ts-left ryan-erc-timestamp-last-inserted-left)
      (goto-char (point-min))
      (erc-put-text-property 0 (length ts-left) 'field 'erc-timestamp ts-left)
      (insert ts-left)
      (setq ryan-erc-timestamp-last-inserted-left ts-left))
    ;; insert right timestamp
    (let (
          (erc-timestamp-only-if-changed-flag t)
	  (erc-timestamp-last-inserted ryan-erc-timestamp-last-inserted-right))
      (ryan-erc-insert-timestamp-right ts-right)
      (setq ryan-erc-timestamp-last-inserted-right ts-right))))



(provide 'ryan-znc)
