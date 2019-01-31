;;; sound-cues.el --- Attach sound cues to events.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  GitHub user "jcaw"

;; Author: GitHub user "jcaw" <40725916+jcaw@users.noreply.github.com>
;; Keywords: multimedia
;; Homepage: https://github.com/jcaw/sound-cues.el

;; Modifications can be found in the git repository.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sound cues can be attached to any function (or hook). Want to know when
;; another package completes a slow, asynchronous function? Just attach a sound
;; cue to that function. Want to know when it completes a background process?
;; Add a sound cue to the callback function.
;;
;; See the README for more.

;;; Code:


(require 'cl-lib)
;; Emacs has a built-in `play-sound' function, but it blocks. Use `sound-wav'
;; for async sounds.
(require 'sound-wav)
(when (memq system-type '(windows-nt ms-dos cygwin))
  ;; On Windows, `sound-wav' starts a Powershell process in the background to
  ;; get around slow process interop (sounds take a while to play otherwise).
  ;; However, it only does this when the `powershell' package is installed - it
  ;; will work without it.
  ;;
  ;; So: force require `powershell' on Windows to ensure sounds are snappy.
  (require 'powershell)
  ;; TODO: Maybe also require powershell to be available before requiring the
  ;; package? E.g:
  ;;   (executable-find "powershell")
  )


;;;###autoload
(defgroup sound-cues nil
  "Play sound cues on certain events."
  :prefix "sound-cues-"
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


;;;###autoload
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


;;;###autoload
(defun sound-cues-demo-sounds ()
  "Demo each of the inbuilt sounds."
  (interactive)
  (mapc (lambda (sound)
          (message "Playing inbuilt sound: `%s'" sound)
          (sound-cues-play-sound sound :block t)
          (sleep-for 2))
        sound-cues-inbuilt-sounds)
  (message "Done!"))


;;;###autoload
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


;;;###autoload
(cl-defun sound-cues-play-sound (sound &key (block nil))
  "Play some `sound'.

If sound is a symbol, it will attempt to play the path of the
inbuilt sound (`sound-cues-inbuilt-sounds') that matches this
path.

If sound is a string, it will attempt to play it as a file path.

Set `:block' to `t' to block Emacs while playing the sound.
Otherwise, the sound will be played asynchronously. Note that
only one sound may be playing at a time.

Please note that when Emacs is blocked while playing the sound,
`C-g' interruption will not work."
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
          (user-error "`%s' is not an inbuilt sound." sound))
        (format "%s%s.wav" sound-cues--sounds-directory sound))
    sound))


(defun sound-cues--play-sound-blocking (file)
  "Play a sound and block Emacs until it is complete."
  (play-sound `(sound :file ,file)))


;; ---------------------------------------------------------------------


(defvar sound-cues-registered-cues
  '()
  "All the sound cues currently registered.")


(defun sound-cues--construct-sound-lambda (sound-file)
  "Construct a lambda function that plays `sound-file'.

This function will not block - the sound will be played asynchronously."
  `(lambda (&rest _)
     (sound-cues-play-sound
      ;; Pass the current value of sound-file, don't use the
      ;; variable.
      ,sound-file
      ;; Alerts should be played asynchronously.
      :block nil)))


;;;###autoload
(cl-defun sound-cues-add-cue (func sound)
  "Add a sound cue to a particular function.

The `SOUND' will play when the function `FUNC' completes.

`SOUND' - May be an inbuilt sound (a symbol) or the path to a
          sound file (a string).

         `sound-cues-inbuilt-sounds' contains all inbuilt sounds.
         Call M-x `sound-cues-demo-sounds' to hear all available
         inbuilt sounds.

`FUNC' - Any function (or callable). `SOUND' will be played
         asynchronously when the function completes.

Please note, only one sound can be played at a time. If one sound
cue is already playing, other cues will be skipped."
  ;; TODO: Maybe have sound cues before or after functions? Possibly on hooks?
  (let ((registered-cue (assoc func sound-cues-registered-cues))
        (sound-file (sound-cues--normalise-sound-file sound)))
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
    (let ((play-sound-func (sound-cues--construct-sound-lambda sound-file)))
      (advice-add func :after play-sound-func)
      (push (list func
                  `((sound ,sound)
                    (advice ,play-sound-func)))
            sound-cues-registered-cues))))


;;;###autoload
(defun sound-cues-remove-cue (func)
  "Remove the sound cue from a particular function, `FUNC'."
  (let* ((registered-cue (assoc func sound-cues-registered-cues))
         (data (nth 1 registered-cue))
         (advice-to-remove (nth 1 (assoc 'advice data))))
    (advice-remove func advice-to-remove)
    (setq sound-cues-registered-cues
          (remove registered-cue sound-cues-registered-cues))))


;;;###autoload
(defun sound-cues-remove-all-cues ()
  "Remove all sound cues from all functions (and hooks)."
  ;; Remove function cues
  (mapc (lambda (cue)
          (sound-cues-remove-cue (car cue)))
        sound-cues-registered-cues)
  ;; Remove cues on hooks
  (mapc (lambda (cue)
          (sound-cues-remove-cues-from-hook (car cue)))
        sound-cues-registered-hook-cues))


(defvar sound-cues-registered-hook-cues
  '()
  "All the sound cues currently registered on hooks.")


;;;###autoload
(defun sound-cues-add-cue-to-hook (hook sound)
  "Add a sound cue to some `HOOK'.

`SOUND' will be played when `HOOK' is run.

`SOUND' - May be an inbuilt sound (a symbol) or the path to a
          sound file (a string).

         `sound-cues-inbuilt-sounds' contains all inbuilt sounds.
         Call M-x `sound-cues-demo-sounds' to hear all available
         inbuilt sounds.

`HOOK' - A hook. `SOUND' will be played asynchronously when the
         hook is run.

Please note, only one sound can be played at a time. If one sound
cue is already playing, other cues will be skipped."
  (let* ((registered-cue (assoc hook sound-cues-registered-hook-cues))
         (sound-file (sound-cues--normalise-sound-file sound))
         (play-sound-func (sound-cues--construct-sound-lambda sound-file)))
    ;; Ensure the sound file exists.
    (unless (file-exists-p sound-file)
      (error "Sound file could not be found. File: '%s'" sound-file))

    ;; If this function has a sound cue already, remove it and warn the user.
    (when registered-cue
      (display-warning
       "sound-cues"
       (format "Hook `%s' already has a sound cue registered. Removing it. Old cue: '%s'"
               hook
               ;; This is the data
               (assoc 'sound (nth 1 registered-cue))))
      (sound-cues-remove-cue-from-hook hook))

    ;; Add lambda to the hook
    (add-hook hook play-sound-func)
    (push (list hook
                `((sound ,sound)
                  (func ,play-sound-func)))
          sound-cues-registered-hook-cues)))


;;;###autoload
(defun sound-cues-remove-cue-from-hook (hook)
  "Remove the sound cue attached to `HOOK'."
  (let* ((registered-cue (assoc hook sound-cues-registered-hook-cues))
         (data (nth 1 registered-cue))
         (play-sound-func (nth 1 (assoc 'func data))))
    (remove-hook hook play-sound-func)
    (setq sound-cues-registered-hook-cues
          (remove registered-cue sound-cues-registered-hook-cues))))


;; Misc TODOs
;; -----------
;;
;; TODOs are ranked.
;;   1 - Need to implement
;;   2 - Want to implement
;;   3 - Might be worth implementing
;;
;; TODO 3: Possibly queue sound alerts so overlapping sounds still play?
;; TODO 3: Text-to-speech cues (e.g. "traad finished", "traad error").
;; TODO 2: Volume control.
;; TODO 1: Cues on hooks.
;; TODO 1: Make this package more robust to errors when playing sounds - don't
;; make it crash the process flow on failure.


(provide 'sound-cues)
;;; sound-cues.el ends here.
