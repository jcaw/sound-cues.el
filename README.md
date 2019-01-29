# sound-cues.el

## Introduction
Make Emacs play sound cues to tell the user when things happen. 

Sound cues can be attached to any function. Want to know when another package completes a slow, asynchronous function? Just attach a sound cue to that function. Want to know when it completes a background process? Add a sound cue to the callback function. 

Adding cues is easy:
```emacs-lisp
(sound-cues-add-cue sound function)
```

The sound will then play each time the function completes.

Some example uses:

- Play a sound when Emacs has finished loading.
- Notify the user when a background process returns.
- Notify the user when an asynchronous function completes.
- Play a buzzer when a function calls its "failure" callback, play a success sound when it calls its "success" callback.

## Usage

### Installation
`sound-cues.el` isn't on MELPA yet. For now, I recommend installing directly from GitHub with `quelpa`. Evaluate the following:
```emacs-lisp
(package-install 'quelpa)
(require 'quelpa)
(quelpa '(sound-cues
          :fetcher git 
          :url "https://github.com/jcaw/sound-cues.el"))
```

Then require as normal:
```emacs-lisp
(require 'sound-cues)
```

Alternatively, you can clone the repo and add the directory to the load-path yourself.

#### Test Your Speakers

Once installed, test that sound cues can play successfully with:
```emacs-lisp
M-x sound-cues-test-speakers
```

### Adding Cues

Add sound cues to a function with:
```emacs-lisp
;; You can use inbuilt sounds
(sound-cues-add-cue 'success 'some-function)      ; `success' is an inbuilt sound
(sound-cues-add-cue 'failure 'another-function)   ; `failure' is another inbuilt sound

;; Use your own sound files too
(sound-cues-add-cue "/path/to/sound-file.wav" 'third-function)              
```

The cue will play when the function completes. Note that only one sound cue may be attached to each function.

The cue can be an inbuilt sound (pass a symbol to use an inbuilt sound) or a path to a sound file (as a string). Sound files must be in WAV format.

### Removing Cues

Remove cues with:
```emacs-lisp
(sound-cues-remove-cue)        ; Remove cue from one function.
(sound-cues-remove-all-cues)   ; Remove cues from all functions.
```

## Built-In Sounds
`sound-cues.el` contains a number of built-in sound effects, listed in `sound-cues-inbuilt-sounds`. Here's a list of available sounds:

- `success`
- `failure`
- `alert`
- `startup`
- `negative-beep`
- `success-bells-major`
- `success-bells-minor`
- `ooh-yeah`
- `womp-womp-trombone`

You can demo the available sounds with `M-x sound-cues-demo-sounds`. All sounds are licensed under the Creative Commons.
