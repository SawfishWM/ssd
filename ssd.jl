;;
;; ssd.jl - Sawfis-Session-Dialog
;;
;; (c) 2011 Christopher Roy Bratusek <nano@tuxfamily.org>
;;
;; licensed under GNU GPL v2+
;;

(require 'rep)
(require 'rep.system)
(require 'rep.io.files)
(require 'gui.gtk-2.gtk)

(define (usage)
  (write standard-output "\
usage: ssd OPT

where OPT is one of:

	--logout        Logout from current session
	--reboot        Reboot machine
	--shutdown      Shutdown machine
	--lockdown      Lockdown display
	--suspend       Suspend machine (suspend to RAM)
	--hibernate     Hibernate machine (suspend to disk)
	--setup         Configure commands\n"))

(define (setup from-ui)
  ;; init widgets
  (define swindow (gtk-window-new 'toplevel))
  (gtk-container-set-border-width swindow 10)
  (gtk-window-set-title swindow "Sawfish-Session-Dialog")
  (gtk-window-set-wmclass swindow "SSD Setup" "ssd setup")
  (gtk-window-set-position swindow 'center)
  (gtk-window-set-icon-from-file swindow "icons/ssd.png")

  (define do-save (gtk-button-new-from-stock "gtk-save"))
  (define do-clear (gtk-button-new-from-stock "gtk-clear"))

  (define vbox (gtk-vbox-new nil 2))
  (gtk-container-add swindow vbox)

  (define logout-box (gtk-hbox-new nil 2))
  (define logout-label (gtk-label-new "Logout command:"))
  (define logout-entry (gtk-entry-new))
  (gtk-box-pack-start logout-box logout-label)
  (gtk-box-pack-start logout-box logout-entry)
  (gtk-box-pack-start vbox logout-box)
  (when logout-cmd
    (gtk-entry-set-text logout-entry logout-cmd))

  (define reboot-box (gtk-hbox-new nil 2))
  (define reboot-label (gtk-label-new "Reboot command:"))
  (define reboot-entry (gtk-entry-new))
  (gtk-box-pack-start reboot-box reboot-label)
  (gtk-box-pack-start reboot-box reboot-entry)
  (gtk-box-pack-start vbox reboot-box)
  (when reboot-cmd
    (gtk-entry-set-text reboot-entry reboot-cmd))

  (define shutdown-box (gtk-hbox-new nil 2))
  (define shutdown-label (gtk-label-new "Shutdown command:"))
  (define shutdown-entry (gtk-entry-new))
  (gtk-box-pack-start shutdown-box shutdown-label)
  (gtk-box-pack-start shutdown-box shutdown-entry)
  (gtk-box-pack-start vbox shutdown-box)
  (when shutdown-cmd
    (gtk-entry-set-text shutdown-entry shutdown-cmd))

  (define lockdown-box (gtk-hbox-new nil 2))
  (define lockdown-label (gtk-label-new "Lockdown command:"))
  (define lockdown-entry (gtk-entry-new))
  (gtk-box-pack-start lockdown-box lockdown-label)
  (gtk-box-pack-start lockdown-box lockdown-entry)
  (gtk-box-pack-start vbox lockdown-box)
  (when lockdown-cmd
    (gtk-entry-set-text lockdown-entry lockdown-cmd))

  (define suspend-box (gtk-hbox-new nil 2))
  (define suspend-label (gtk-label-new "Suspend command:"))
  (define suspend-entry (gtk-entry-new))
  (gtk-box-pack-start suspend-box suspend-label)
  (gtk-box-pack-start suspend-box suspend-entry)
  (gtk-box-pack-start vbox suspend-box)
  (when suspend-cmd
    (gtk-entry-set-text suspend-entry suspend-cmd))

  (define hibernate-box (gtk-hbox-new nil 2))
  (define hibernate-label (gtk-label-new "Hibernate command:"))
  (define hibernate-entry (gtk-entry-new))
  (gtk-box-pack-start hibernate-box hibernate-label)
  (gtk-box-pack-start hibernate-box hibernate-entry)
  (gtk-box-pack-start vbox hibernate-box)
  (when hibernate-cmd
    (gtk-entry-set-text hibernate-entry hibernate-cmd))

  (define button-box (gtk-hbutton-box-new))
  (gtk-button-box-set-layout button-box 'center)

  (gtk-box-pack-start vbox button-box)
  (gtk-box-pack-start button-box do-clear)
  (gtk-box-pack-start button-box do-save)

  ;; connect signals
  (if from-ui
      (g-signal-connect swindow "delete_event"
        (lambda (w) (gtk-widget-destroy swindow)))
    (g-signal-connect swindow "delete_event"
      (lambda (w) (throw 'quit 0))))

  (define (save-config)
    (let ((file (open-file "~/.ssdrc" 'write)))
      (unwind-protect
        (format file ";; Sawfish-Session-Dialog configuration\n
(defvar-setq logout-cmd \"%s\")
(defvar-setq reboot-cmd \"%s\")
(defvar-setq shutdown-cmd \"%s\")
(defvar-setq lockdown-cmd \"%s\")
(defvar-setq suspend-cmd \"%s\")
(defvar-setq hibernate-cmd \"%s\")" (gtk-entry-get-text logout-entry)
                                    (gtk-entry-get-text reboot-entry)
				    (gtk-entry-get-text shutdown-entry)
				    (gtk-entry-get-text lockdown-entry)
				    (gtk-entry-get-text suspend-entry)
				    (gtk-entry-get-text hibernate-entry))
	(close-file file))))

  (if from-ui
      (g-signal-connect do-save "pressed"
        (lambda (w) ((save-config)
		     (gtk-widget-destroy swindow))))
    (g-signal-connect do-save "pressed"
        (lambda (w) ((save-config)
		     (throw 'quit 0)))))

  (g-signal-connect do-clear "pressed"
    (lambda ()
      (gtk-entry-set-text logout-entry "")
      (gtk-entry-set-text reboot-entry "")
      (gtk-entry-set-text shutdown-entry "")
      (gtk-entry-set-text lockdown-entry "")
      (gtk-entry-set-text suspend-entry "")
      (gtk-entry-set-text hibernate-entry "")))

  (gtk-widget-show-all swindow)

  (unless from-ui
    (setq interrup-mode 'exit)
    (recursive-edit)))

(define logout-cmd)
(define reboot-cmd)
(define shutdown-cmd)
(define lockdown-cmd)
(define suspend-cmd)
(define hibernate-cmd)

;; load config-file
(when (file-exists-p "~/.ssdrc")
  (load "~/.ssdrc" t t t))

;; make sure images are shown
(gtk-rc-parse-string "gtk-button-images = 1")

(define (main)
  
  ;; init widgets
  (define window (gtk-window-new 'toplevel))
  (gtk-container-set-border-width window 10)
  (gtk-window-set-title window "Sawfish-Session-Dialog")
  (gtk-window-set-wmclass window "Sawfish-Session-Dialog"
			  "sawfish-session-dialog")
  (gtk-window-set-position window 'center)
  (gtk-window-set-icon-from-file window "icons/ssd.png")

  (define do-exit (gtk-button-new-from-stock "gtk-close"))
  (define do-edit (gtk-button-new-from-stock "gtk-edit"))

  (define do-logout (gtk-button-new-with-label "Logout"))
  (define img-logout (gtk-image-new-from-file "icons/logout.png"))
  (gtk-button-set-image do-logout img-logout)

  (define do-reboot (gtk-button-new-with-label "Reboot"))
  (define img-reboot (gtk-image-new-from-file "icons/reboot.png"))
  (gtk-button-set-image do-reboot img-reboot)

  (define do-shutdown (gtk-button-new-with-label "Shutdown"))
  (define img-shutdown (gtk-image-new-from-file "icons/shutdown.png"))
  (gtk-button-set-image do-shutdown img-shutdown)

  (define do-lockdown (gtk-button-new-with-label "Lockdown"))
  (define img-lockdown (gtk-image-new-from-file "icons/lock.png"))
  (gtk-button-set-image do-lockdown img-lockdown)

  (define do-suspend (gtk-button-new-with-label "Suspend"))
  (define img-suspend (gtk-image-new-from-file "icons/suspend.png"))
  (gtk-button-set-image do-suspend img-suspend)

  (define do-hibernate (gtk-button-new-with-label "Hibernate"))
  (define img-hibernate (gtk-image-new-from-file "icons/hibernate.png"))
  (gtk-button-set-image do-hibernate img-hibernate)

  (define vbox (gtk-vbox-new t 2))

  (define top-button-box (gtk-hbutton-box-new))
  (gtk-button-box-set-layout top-button-box 'spread)

  (define middle-button-box (gtk-hbutton-box-new))
  (gtk-button-box-set-layout middle-button-box 'spread)

  (define bottom-button-box (gtk-hbutton-box-new))
  (gtk-button-box-set-layout bottom-button-box 'center)

  (gtk-container-add window vbox)

  (gtk-box-pack-start vbox top-button-box)
  (gtk-box-pack-start top-button-box do-logout)
  (gtk-box-pack-start top-button-box do-reboot)
  (gtk-box-pack-start top-button-box do-shutdown)

  (gtk-box-pack-start vbox middle-button-box)
  (gtk-box-pack-start middle-button-box do-lockdown)
  (gtk-box-pack-start middle-button-box do-suspend)
  (gtk-box-pack-start middle-button-box do-hibernate)

  (gtk-box-pack-start vbox bottom-button-box)
  (gtk-box-pack-start bottom-button-box do-edit)
  (gtk-box-pack-start bottom-button-box do-exit)

  ;; connect signals
  (g-signal-connect window "delete_event"
    (lambda (w) (throw 'quit 0)))

  (g-signal-connect do-exit "pressed"
    (lambda (w) (throw 'quit 0)))

  (g-signal-connect do-edit "pressed"
    (lambda (w) (setup t)))

  (define (not-empty sym)
    (if (and sym
	     (not (eq sym "")))
        nil
      t))

  (if (not-empty logout-cmd)
      (g-signal-connect do-logout "pressed"
	(lambda () (system (concat logout-cmd " &"))))
    (gtk-widget-set-sensitive do-logout nil))

  (if (not-empty reboot-cmd)
      (g-signal-connect do-reboot "pressed"
	(lambda () (system (concat reboot-cmd " &"))))
    (gtk-widget-set-sensitive do-reboot nil))

  (if (not-empty shutdown-cmd)
      (g-signal-connect do-shutdown "pressed"
	(lambda () (system (concat shutdown-cmd " &"))))
    (gtk-widget-set-sensitive do-shutdown nil))

  (if (not-empty lockdown-cmd)
      (g-signal-connect do-lockdown "pressed"
	(lambda () (system (concat lockdown-cmd " &"))))
    (gtk-widget-set-sensitive do-lockdown nil))

  (if (not-empty suspend-cmd)
      (g-signal-connect do-suspend "pressed"
	(lambda () (system (concat suspend-cmd " &"))))
    (gtk-widget-set-sensitive do-suspend nil))

  (if (not-empty hibernate-cmd)
      (g-signal-connect do-hibernate "pressed"
	(lambda () (system (concat hibernate-cmd " &"))))
    (gtk-widget-set-sensitive do-hibernate nil))

  (gtk-widget-show-all window)

  (setq interrup-mode 'exit)
  (recursive-edit))

;; get-opts
(when (get-command-line-option "--help")
  (usage)
  (throw 'quit 0))

(when (get-command-line-option "--setup")
  (setup nil)
  (throw 'quit 0))

(when (get-command-line-option "--logout")
  (if logout-cmd
      (progn
	(system (concat logout-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for logout set."))
    (throw 'quit 1))

(when (get-command-line-option "--reboot")
  (if reboot-cmd
      (progn
	(system (concat reboot-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for reboot set."))
    (throw 'quit 1))

(when (get-command-line-option "--shutdown")
  (if shutdown-cmd
      (progn
	(system (concat shutdown-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for shutdown set."))
    (throw 'quit 1))

(when (get-command-line-option "--lockdown")
  (if lockdown-cmd
      (progn
	(system (concat lockdown-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for lockdown set."))
    (throw 'quit 1))

(when (get-command-line-option "--suspend")
  (if suspend-cmd
      (progn
	(system (concat suspend-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for suspend set."))
    (throw 'quit 1))

(when (get-command-line-option "--hibernate")
  (if hibernate-cmd
      (progn
	(system (concat hibernate-cmd " &"))
	(throw 'quit 0))
    (write standard-output "No command for hibernate set."))
    (throw 'quit 1))

(main)
