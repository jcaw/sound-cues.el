;; This is the recipe structure for Quelpa, MELPA, Spacemacs etc.

(sound-cues :fetcher github
            :repo "jcaw/sound-cues.el"
            ;; Must explicitly download the sounds folder (and README).
            :files ("*.el" "README.md" "sounds"))
