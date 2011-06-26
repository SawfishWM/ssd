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
	--hibernate     Hibernate machine (suspend to disk)\n"))

(define logout-cmd)
(define reboot-cmd)
(define shutdown-cmd)
(define lockdown-cmd)
(define suspend-cmd)
(define hibernate-cmd)

;; load config-file
(when (file-exists-p "~/.ssdrc")
  (load "~/.ssdrc" t t t))

(define (main)
  
  ;; make sure images are shown
  (gtk-rc-parse-string "gtk-button-images = 1")

  ;; init widgets
  (define window (gtk-window-new 'toplevel))
  (gtk-container-set-border-width window 10)
  (gtk-window-set-title window "Sawfish-Session-Dialog")
  (gtk-window-set-wmclass window "Sawfish-Session-Dialog"
			  "sawfish-session-dialog")
  (gtk-window-set-position window 'center)
  (gtk-window-set-icon-from-file window "icons/ssd.png")

  (define do-exit (gtk-button-new-from-stock "gtk-close"))

  (define do-logout (gtk-button-new-with-label "Logout"))
  (define img-logout (gtk-image-new-from-file "icons/logout.png"))
  (gtk-button-set-image do-logout img-logout)

  (define do-reboot (gtk-button-new-with-label "Restart"))
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
  (gtk-box-pack-start bottom-button-box do-exit)

  ;; connect signals
  (g-signal-connect window "delete_event"
    (lambda (w) (throw 'quit 0)))

  (g-signal-connect do-exit "pressed"
    (lambda (w) (throw 'quit 0)))

  (if logout-cmd
      (g-signal-connect do-logout "pressed"
	(lambda () (system (concat logout-cmd " &"))))
    (gtk-widget-set-sensitive do-logout nil))

  (if reboot-cmd
      (g-signal-connect do-reboot "pressed"
	(lambda () (system (concat reboot-cmd " &"))))
    (gtk-widget-set-sensitive do-reboot nil))

  (if shutdown-cmd
      (g-signal-connect do-shutdown "pressed"
	(lambda () (system (concat shutdown-cmd " &"))))
    (gtk-widget-set-sensitive do-shutdown nil))

  (if lockdown-cmd
      (g-signal-connect do-lockdown "pressed"
	(lambda () (system (concat lockdown-cmd " &"))))
    (gtk-widget-set-sensitive do-lockdown nil))

  (if suspend-cmd
      (g-signal-connect do-suspend "pressed"
	(lambda () (system (concat suspend-cmd " &"))))
    (gtk-widget-set-sensitive do-suspend nil))

  (if hibernate-cmd
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
