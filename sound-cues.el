(defvar sound-cues-default-volume 1
  "The default volume to play sound cues. Should be in range 0-1.")


(defvar sound-cues--sounds-directory
  (concat (file-name-directory
           (or
            ;; `load-file-name' should point to this file when loading.
            load-file-name
            ;; If not loaded as a package, use the buffer for this file instead.
            buffer-file-name))
          "sounds/"))


(play-sound )



(provide 'sound-cues)
