(structure ()

    (open rep
	  rep.io.files
	  rep.system
	  gui.gtk-2.gtk)

  (define builder (gtk-builder-new))
  
  (define logout-cmd)
  (define restart-cmd)
  (define shutdown-cmd)
  (define lockdown-cmd)
  (define suspend-cmd)
  (define hibernate-cmd)

  ;; load config-file
  (when (file-exists-p "~/.ssdrc")
    (load "~/.ssdrc" t t t))

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

  (define do-restart (gtk-button-new-with-label "Restart"))
  (define img-restart (gtk-image-new-from-file "icons/restart.png"))
  (gtk-button-set-image do-restart img-restart)

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
  (gtk-box-pack-start top-button-box do-restart)
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

  (if restart-cmd
      (g-signal-connect do-restart "pressed"
	(lambda () (system (concat restart-cmd " &"))))
    (gtk-widget-set-sensitive do-restart nil))

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

  (setq interrupt-mode 'exit)
  (recursive-edit))
