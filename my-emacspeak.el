(when *run-emacspeak*
  (defvar folding-mode nil)
  (defconst *emacspeak-src-dir* (if (getenv "EMACSPEAK_DIR")
                                    (expand-file-name
                                     (getenv "EMACSPEAK_DIR"))
                                  (expand-file-name "~/git/emacspeak/trunk"))
    "Where emacs will find the emacspeak distro")

    (defconst *dtk-program* (if (getenv "DTK_PROGRAM")
                                (getenv "DTK_PROGRAM")
                              (if (spacemacs/system-is-mac)
                                  "mac"
                                "espeak")))
    (require 'info)
    (add-to-list 'Info-additional-directory-list
                 (expand-file-name "info"  *emacspeak-src-dir*)))

(when (and *run-emacspeak*
           (not noninteractive)
           (not (featurep 'emacspeak)))
  (add-to-list 'load-path *emacspeak-src-dir*)
  (setenv "EMACSPEAK_DIR" *emacspeak-src-dir*)
  (setenv "DTK_PROGRAM" *dtk-program*)
  (setq dtk-program *dtk-program*
        dtk-use-tones nil
        emacspeak-erc-my-nick "theophilusx"
        emacspeak-erc-speak-all-participants t
	      emacspeak-eww-inhibit-images nil
        emacspeak-mail-alert nil
        emacspeak-play-emacspeak-startup-icon nil
        emacspeak-vm-use-raman-settings nil
        emacspeak-play-program (expand-file-name "~/bin/play")
        emacpseak-play-args nil
        mac-default-speech-rate 360
        outloud-default-speech-rate 90
        espeak-default-speech-rate 250
        sox-play "/usr/bin/play"
        emacspeak-soxplay-command "/usr/bin/play -v 1.2 %s earwax &"
        tts-default-speech-rate 90)
  (add-hook 'emacspeak-startup-hook
            (lambda ()
              ;; (dtk-set-rate tts-default-speech-rate 1)
              (dtk-interp-sync)
              (emacspeak-sounds-select-theme "3d/")))

  (load-file (concat *emacspeak-src-dir* "/lisp/emacspeak-setup.el")))

(defadvice evil-next-visual-line (after emacspeak pre act comp)
  (emacspeak-speak-line))

(defadvice evil-previous-visual-line (after emacspeak pre act comp)
  (emacspeak-speak-line))

(provide 'my-emacspeak)
