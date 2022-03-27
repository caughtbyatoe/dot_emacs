;;
;; TCP server
;;   The most common need for this is when running shells under tramp mode.
;;   In that case, invocations of emacsclient will need to specify the
;;   server file and the tramp prefix in addition to the file being edited.
;;   For example,
;;      emacsclient --server-file=/PATH/TO/SERVER/server -T /ssh:HOST: file_to_edit
;;
(defun start-tcp-server ()
  "Start a tcp emacs server listening on the given ip address"
  (interactive)
  ;; This is the ip address that emacsclient invocations will use to
  ;; connect.  Given the widespread use of VPN and multiple network interfaces
  ;; it isn't clear to me how to  automate its lookup.  So I just ask for it.
  (setq server-host (read-string "server ip address : "))
  (setq server-use-tcp 1)
  ;; it can be useful to put the server file in a NFS location
  ;; that can be accessed by emacsclient users
  ;; the default location is ~/.emacs.d/server
  ;; (setq server-auth-dir "/NFS/PATH/server")
  (server-start))
