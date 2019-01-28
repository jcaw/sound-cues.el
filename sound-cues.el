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
          (sound-cues--play-sound sound :block t)
          (sleep-for 2))
        sound-cues-inbuilt-sounds)
  (message "Done!"))


(defun sound-cues-test-speakers ()
  "Play the `startup' sound to test that sound cues are working."
  (interactive)
  (message "Playing startup sound.")
  (sound-cues--play-sound 'startup))


(cl-defun sound-cues--play-sound (sound &key (block nil))
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


(provide 'sound-cues)
;;; sound-cues.el ends here.
