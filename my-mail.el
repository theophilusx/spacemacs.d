(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-debug-info nil
      smtpmail-debug-verb nil
      smtpmail-smtp-service 587
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-queue-mail nil ;; start in normal mode
      smtpmail-queue-dir   (expand-file-name "~/Maildir/queue/cur")
      message-kill-buffer-on-exit t)

(require 'smtpmail)

(setq mu4e-info "/usr/local/share/info")
(add-to-list 'Info-additional-directory-list (expand-file-name mu4e-info))

(when *run-emacspeak*
  (require 'emacspeak-mu4e))

(setq tx-gmail-email "theophilusx@gmail.com"
	    tx-sheepcrc-email "tcross8@une.edu.au"
	    tx-hotmail-email "blind-bat@hotmail.com")

(setq mu4e-attachment-dir (expand-file-name "~/Desktop")
	    mu4e-get-mail-command "mbsync -a"
	    mu4e-hide-index-messages t
	    mu4e-headers-include-related nil
	    mu4e-change-filenames-when-moving t
	    mu4e-context-policy 'pick-first
	    mu4e-compose-context-policy nil
	    mu4e-use-fancy-chars t
	    mu4e-view-fields '(:from :to :cc :subject :date :attachments)
	    mu4e-view-scroll-to-next nil
	    mu4e-view-show-addresses t
	    mu4e-view-use-gnus t
	    mu4e-update-interval (* 60 10)
      org-mu4e-link-query-in-headers-mode nil)

(add-hook 'message-mode-hook 'turn-on-orgtbl)

(require 'mu4e)

(setq mu4e-bookmarks '())
(add-to-list 'mu4e-bookmarks
		         (make-mu4e-bookmark
		          :name "Last 7 days"
		          :query "date:7d..now AND NOT flag:trashed"
		          :key ?w))
(add-to-list 'mu4e-bookmarks
		         (make-mu4e-bookmark
		          :name "Messages with images"
		          :query "mime:image/* AND NOT flag:trashed"
		          :key ?p))
(add-to-list 'mu4e-bookmarks
		         (make-mu4e-bookmark
		          :name "Today's messages"
		          :query "date:today..now AND NOT flag:trashed"
		          :key ?t))
(add-to-list 'mu4e-bookmarks
		         (make-mu4e-bookmark
		          :name  "Unread messages"
		          :query (if (string= system-name "arch-vbox")
			                   (concat "(maildir:/gmail/INBOX OR "
				                         "maildir:/outlook/INBOX) AND "
				                         "flag:unread AND NOT flag:trashed")
			                 (concat "(maildir:/gmail/INBOX OR "
				                       "maildir:/outlook/INBOX OR "
				                       "maildir:/sheep/INBOX) AND "
				                       "flag:unread AND NOT flag:trashed"))
		          :key ?u))


(setq mu4e-contexts '())
(add-to-list 'mu4e-contexts
		         (make-mu4e-context
		          :name "Outlook"
		          :enter-func
		          (lambda ()
		            (mu4e-message "Entering Outlook context"))
		          :leave-func
		          (lambda ()
		            (mu4e-message "Leaving Outlook context"))
		          ;; we match based on the contact-fields of the message
		          :match-func
		          (lambda (msg)
		            (when msg
		              (mu4e-message-contact-field-matches msg
							                                        :to
							                                        "blind-bat@hotmail.com")))
		          :vars '((user-mail-address . "blind-bat@hotmail.com")
			                (user-full-name . "Tim Cross")
			                (mu4e-compose-signature . (concat "Tim Cross\n"))
			                (mu4e-drafts-folder . "/outlook/Drafts")
			                (mu4e-sent-folder . "/outlook/Sent Mail")
			                (mu4e-trash-folder . "/outlook/Deleted Items")
			                (mu4e-refile-folder . "/outlook/Archive")
			                (mu4e-maildir-shortcuts . (("/outlook/INBOX" . ?i)
						                                     ("/outlook/Sent Mail" . ?s)
						                                     ("/outlook/Archive" . ?a)
						                                     ("/outlook/Drafts" . ?d)))
			                (smtpmail-smtp-server . "smtp-mail.outlook.com")
			                (smtpmail-smtp-service . 587)
			                (mu4e-sent-messages-behavior . delete))))
(if (string= system-name "tim-desktop")
	  (add-to-list 'mu4e-contexts
		             (make-mu4e-context
		              :name "Sheep"
		              :enter-func
		              (lambda ()
			              (mu4e-message "Entering Sheep context"))
		              :leave-func
		              (lambda ()
			              (mu4e-message "Leaving Sheep context"))
		              ;; we match based on the contact-fields of the message
		              :match-func
		              (lambda (msg)
			              (when msg
			                (mu4e-message-contact-field-matches msg
							                                            :to
							                                            "tcross8@une.edu.au")))
		              :vars
		              '((user-mail-address . "tcross8@une.edu.au")
			              (user-full-name . "Tim Cross")
			              (mu4e-compose-signature .
						                                (concat
						                                 "Tim Cross\n"
						                                 "DBA/Developer - "
						                                 "Livestock App Building\n"
						                                 "School of Science and "
						                                 "Technology\n"
						                                 "Room 253 Booth Block (C027), "
						                                 "Univrsity of New England\n"))
			              (mu4e-drafts-folder . "/sheep/Drafts")
			              (mu4e-sent-folder . "/sheep/Sent Items")
			              (mu4e-trash-folder . "/sheep/Deleted Items")
			              (mu4e-refile-folder . "/sheep/Archive")
			              (mu4e-maildir-shortcuts . (("/sheep/INBOX" . ?i)
						                                   ("/sheep/Sent Items" . ?s)
						                                   ("/sheep/Archive" . ?a)
						                                   ("/sheep/Drafts" . ?d)))
			              (smtpmail-smtp-server . "mailhub.une.edu.au")
			              (smtpmail-smtp-service . 25)
			              (mu4e-sent-messages-behavior . sent)))))
(add-to-list 'mu4e-contexts
		         (make-mu4e-context
		          :name "Private"
		          :enter-func (lambda ()
				                    (mu4e-message "Entering Private context"))
		          :leave-func
		          (lambda ()
		            (mu4e-message "Leaving Private context"))
		          ;; we match based on the contact-fields of the message
		          :match-func
		          (lambda (msg)
		            (when msg
		              (mu4e-message-contact-field-matches msg :to "theophilusx@gmail.com")))
		          :vars
		          '((user-mail-address . "theophilusx@gmail.com")
		            (user-full-name . "Tim Cross")
		            (mu4e-compose-signature . (concat "Tim Cross\n"))
		            (mu4e-drafts-folder . "/gmail/Drafts")
		            (mu4e-sent-folder . "/gmail/Sent Mail")
		            (mu4e-trash-folder . "/gmail/Bin")
		            (mu4e-refile-folder . "/gmail/All Mail")
		            (mu4e-maildir-shortcuts . (("/gmail/INBOX" . ?i)
					                                 ("/gmail/Sent Mail" . ?s)
					                                 ("/gmail/All Mail" . ?a)
					                                 ("/gmail/Drafts" . ?d)
					                                 ("/gmail/Starred" . ?f)))
		            (smtpmail-smtp-server . "smtp.gmail.com")
		            (smtpmail-smtp-service . 587)
		            (mu4e-sent-messages-behavior . delete))))

(with-eval-after-load 'mu4e-alert
  (setq mu4e-alert-interesting-mail-query
        (if (string= system-name "arch-vbox")
            (concat
             "(maildir:/gmail/INBOX OR "
             "maildir:/outlook/INBOX) AND "
             "flag:unread AND NOT flag:trashed")
          (concat
           "(maildir:/gmail/INBOX OR "
           "maildir:/outlook/INBOX OR "
           "maildir:/sheep/INBOX) AND "
           "flag:unread AND NOT flag:trashed")))
  (mu4e-alert-enable-mode-line-display))

(provide 'my-mail)
