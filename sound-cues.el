(require 'sound-wav)
(when (memq system-type '(windows-nt ms-dos cygwin))
  ;; `sound-wav' Needs a background Powershell on Windows to get around slow
  ;; process interop and play sounds quickly.
  (require 'powershell)
  ;; TODO: Maybe also require powershell to be available before requiring the package?
  ;;   (executable-find "powershell")
  )


(defgroup sound-cues nil
  "Play sound cues on certain events."
  :group 'sound)


;; Current approach doesn't allow us to control volume.
;; (defvar sound-cues-default-volume 1
;;   "The volume with which to play sound cues. Should be in range 0-1.")


(defvar sound-cues--sounds-directory
  (concat (file-name-directory
           (or
            ;; `load-file-name' should point to this file when loading.
            load-file-name
            ;; If not loaded as a package, use the buffer for this file instead.
            buffer-file-name))
          "sounds/"))


(defvar sound-cues-inbuilt-sounds
  '(success
    failure
    alert
    startup
    negative-beep
    success-bells-major
    success-bells-minor
    ooh-yeah
    womp-womp-trombone)
  "Inbuilt sounds that can be used as sound cues.

All sounds are licensed under Creative Commons.")


(defun sound-cues-demo-sounds ()
  "Demo each of the inbuilt sounds."
  (interactive)
  (mapc (lambda (sound)
          (message "Playing inbuilt sound: `%s'" sound)
          (sound-cues-play-sound sound :block t)
          (sleep-for 2))
        sound-cues-inbuilt-sounds)
  (message "Done!"))


(defun sound-cues-test-speakers ()
  "Test whether sound cues are working correctly.

Plays the `startup' sound twice."
  (interactive)
  (message "Playing startup sound (ding dong) - blocking version.")
  (sound-cues-play-sound 'startup :block t)
  (message "Done - will now play non-blocking version.")
  (sleep-for 2)
  (message "Playing startup sound (ding dong) - non-blocking version.")
  (sound-cues-play-sound 'startup :block nil)
  (sleep-for 4)
  (message "You should have heard the startup sound twice. If not, something is wrong."))


(cl-defun sound-cues-play-sound (sound &key (block nil))
  "Play some `sound'.

If sound is a symbol, it will attempt to play the path of the
inbuilt sound (`sound-cues-inbuilt-sounds') that matches this
path.

If sound is a string, it will attempt to play it as a file path.

Set `:block' to `t' to block Emacs while playing the sound.
Otherwise, the sound will be played asynchronously. Note that
only one sound may be playing at a time."
  ;; TODO: Maybe try converting sound to symbol *then* checking membership?
  (let ((sound-file (sound-cues--normalise-sound-file sound)))
    (if block
        (sound-cues--play-sound-blocking sound-file)
      (sound-cues--play-sound-async sound-file))))


(defun sound-cues--play-sound-async (file)
  "Play a sound file asynchronously, so as not to block Emacs."
  ;; This approach didn't work (on Windows) - recording for posterity.
  ;; (async-start (lambda ()
  ;;                (play-sound `(sound :file sound-file))))
  ;; This approach plays asynchronously, but doesn't allow us to control the
  ;; volume.
  (sound-wav-play file))


(defun sound-cues--normalise-sound-file (sound)
  "Turn `sound' into a sound file path.

If sound is a symbol, it will attempt to find the path.

If sound is a string, it will assume it is already a file path."
  (if (symbolp sound)
      (progn
        (unless (member sound sound-cues-inbuilt-sounds)
          (user-error "`%s' is not an inbuilt sound."))
        (format "%s%s.wav" sound-cues--sounds-directory sound))
    sound))


(defun sound-cues--play-sound-blocking (file)
  "Play a sound and block Emacs until it is complete."
  (play-sound `(sound :file ,file)))


;; ---------------------------------------------------------------------


(defvar sound-cues-registered-cues
  '()
  "All the sound cues currently registered.")


(cl-defun sound-cues-add-cue (func sound)
  "Add a sound cue to a particular function.

The `SOUND' will play when the function `FUNC' completes."
  ;; TODO: Maybe have sound cues before or after functions? Possibly on hooks?
  (let ((registered-cue (assoc func sound-cues-registered-cues))
        (sound-file (sound-cues--normalise-sound-file sound)))
    (message "Registered cue: '%s'" registered-cue)
    ;; Ensure the sound file exists.
    (unless (file-exists-p sound-file)
      (error "Sound file could not be found. File: '%s'" sound-file))

    ;; If this function has a sound cue already, remove it and warn the user.
    (when registered-cue
      (display-warning
       "sound-cues"
       (format "Function `%s' already has a sound cue registered. Removing it. Old cue: '%s'"
               func
               ;; This is the data
               (assoc 'sound (nth 1 registered-cue))))
      (sound-cues-remove-cue func))

    ;; Now add the advice.
    (let ((advice `(lambda (&rest _)
                     (sound-cues-play-sound
                      ;; Pass the current value of sound-file, don't use the
                      ;; variable.
                      ,sound-file
                      :block nil))))
      (advice-add func :after advice)
      (push (list func
                  `((sound ,sound-file)
                    (advice ,advice)))
            sound-cues-registered-cues))))


(defun sound-cues-remove-cue (func)
  "Remove the sound cue from a particular function, `FUNC'."
  (let* ((registered-cue (assoc func sound-cues-registered-cues))
         (data (nth 1 registered-cue))
         (advice-to-remove (nth 1 (assoc 'advice data))))
    (advice-remove func advice-to-remove)
    (setq sound-cues-registered-cues
          (remove registered-cue sound-cues-registered-cues))))


(defun sound-cues-remove-all-cues ()
  "Remove all sound cues from all functions."
  (mapc (lambda (cue)
          (sound-cues-remove-cue (car cue)))
        sound-cues-registered-cues))


(provide 'sound-cues)
;;; sound-cues.el ends here.
