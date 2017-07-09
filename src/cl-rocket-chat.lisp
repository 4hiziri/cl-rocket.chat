(in-package :cl-user)
(defpackage cl-rocket-chat
  (:use :cl))
(in-package :cl-rocket-chat)

(ql:quickload 'dexador)
(ql:quickload 'cl-json)
(ql:quickload 'cl-annot)

(cl-annot:enable-annot-syntax)

(defun bool-to-str (bool)
  (if bool
      "true"
      "false"))

(defun concat (&rest str)
  (reduce (lambda (x y) (concatenate 'string x y)) str))

(defstruct reg-info
  :email
  :name ; display name
  :password
  :username
  (:active t)
  (:roles (list "user"))
  (:joinDefaultChannels t)
  (:requirePasswordChange nil)
  (:sendWelcomeEmail nil)
  (:verified nil)
  (:customFields "undefined") ;; {field : value}
  )

;; :TODO tostr?
(defun reg-info-to-alist (reg-info)
  (let ((ret nil))
    (push `(:email ,(reg-info-email reg-info)) ret)
    (push `(:name ,(reg-info-name reg-info)) ret)
    (push `(:password ,(reg-info-password reg-info)) ret)
    (push `(:username ,(reg-info-username reg-info)) ret)
    (push `(:active ,(bool-to-str (reg-info-active reg-info))) ret)
    (push `(:roles ,(list-to-str (reg-info-roles reg-info))) ret)
    (push `(:joinDefaultChannels ,(bool-to-str (reg-info-joinDefaultChannels reg-info))) ret)
    (push `(:requirePasswordChange
	    ,(bool-to-str (reg-info-requirePasswordChange reg-info)))
	  ret)
    (push `(:sendWelcomeEmail ,(bool-to-str (reg-info-sendWelcomeEmail reg-info))) ret)
    (push `(:verified ,(bool-to-str (reg-info-verified reg-info))) ret)
    (push `(:customFields ,(reg-info-customFields reg-info)) ret)
    ret))

(defun list-to-str (list)
  (let ((ret-str "["))
    (dolist (e list)
      (setf ret-str (concat ret-str "'" e "'" ",")))
    (concat (coerce (reverse
		     (cdr (coerce (reverse ret-str) 'list)))
		    'string)
	    "]")))

(defstruct auth-token
  :user-id
  :token)

(defun header (auth-token)
  `(("X-Auth-Token" . ,(auth-token-token auth-token))
    ("X-User-Id" . ,(auth-token-user-id auth-token))))

(defun post-json (url header arg-json-alist)
  (dex:post url
	    :headers (cons '("Content-type" . "application/json") header)
	    :content (cl-json:encode-json-alist-to-string arg-json-alist)))

@export
(defun info (url)
  (cl-json:decode-json-from-string
   (dex:get (concat url "/api/v1/info"))))

;; :TODO def struct login-info
@export
(defun login (url user-name password)
  (flet ((get-user-id (auth-info)
	   (cdr (assoc :user-id (cddr (assoc :data auth-info)))))
	 (get-token (auth-info)
	   (cdr (assoc :auth-token (cdr (assoc :data auth-info))))))
    (let ((auth-info (cl-json:decode-json-from-string
		       (dex:post (concat url "/api/v1/login")
				 :content `(("username" . ,user-name)
					    ("password" . ,password))))))
      (if (string= (cdr (assoc :status auth-info)) "success")
	  (make-auth-token :user-id (get-user-id auth-info)
			   :token (get-token auth-info))))))

@export
(defun logout (url auth-token)
  (let ((ret (cl-json:decode-json-from-string
	      (dex:get (concat url "/api/v1/logout")
		       :headers (header auth-token)))))
    (string= (cdr (assoc :status ret)) "success")))

;; :TODO def struct?
@export
(defun me (url auth-token)
  (cl-json:decode-json-from-string
   (dex:get (concat url "/api/v1/me")
	    :headers (header auth-token))))

;; :TODO test
@export
(defun users-create (url auth-info reg-info)
  (post-json (concat url "/api/v1/users.create")
	     (header auth-info)
	     (cl-json:encode-json-alist-to-string (reg-info-to-alist reg-info))))

(defun get-user-arg (value userid-p)
  (if userid-p
      `("userId" . ,value)
      `("username" . ,value)))

@export
(defun users-create-token (url auth-info username &optional (userid-p nil))
  "get new id and access-token"
  (let* ((arg (get-user-arg username userid-p))
	 (json (cl-json:decode-json-from-string
		(post-json (concat url "/api/v1/users.createToken") (header auth-info) (list arg)))))
    (if (cdr (assoc :success json))
	(make-auth-token :user-id (cdr (assoc :user-id (cdr (assoc :data json))))
			 :token (cdr (assoc :auth-token (cdr (assoc :data json))))))))

;; :TODO test
@export
(defun users-delete (url auth-token userid)
  (let ((result (cl-json:decode-json-from-string (post-json (concat url "/api/v1/users.delete")
							    (header auth-token)
							    `(("userId" . ,userid))))))
    (cdr (assoc :success result))))

@export
(defun users-get-avatar (url username &optional (userid-p nil))
  (let ((arg (get-user-arg username userid-p)))
    (dex:get (concat url "/api/v1/users.getAvatar?" (car arg) "=" (cdr arg)))))

@export
(defun users-get-presence (url auth-token username &optional (userid-p nil))
  "if username is nil, get with no-args"
  ;; neeed?
  (cl-json:decode-json-from-string
   (if username
       (let ((arg (get-user-arg username userid-p)))
	 (dex:get (concat url "/api/v1/users.getPresence?" (car arg) "=" (cdr arg))
		  :headers (header auth-token)))
       (dex:get (concat url "/api/v1/users.getPresence")
		:headers (header auth-token)))))

@export
(defun users-info (url auth-token username &optional (userid-p nil))
  (let* ((arg (get-user-arg username userid-p)))
    (cl-json:decode-json-from-string
     (dex:get (concat url "/api/v1/users.info?" (car arg) "=" (cdr arg))
	      :headers (header auth-token)))))

@export
(defun users-list (url auth-token)
  (cl-json:decode-json-from-string
   (dex:get (concat url "/api/v1/users.list")
	    :headers (header auth-token))))

;; :TODO test
@export
(defun users-register (url reg-info &optional (secret-url nil))
  (let ((args nil))
    (push `("username" . ,(reg-info-username reg-info)) args)
    (push `("email" . ,(reg-info-email reg-info)) args)
    (push `("pass" . ,(reg-info-password reg-info)) args)
    (push `("name" . ,(reg-info-name reg-info)) args)
    (when secret-url (push `("username" . ,(reg-info-username reg-info)) args))
    (cl-json:decode-json-from-string
     (post-json (concat url "/api/v1/users.register") nil args))))

@export
(defun users-reset-avatar (url auth-token username &optional (userid-p nil))
  (let* ((arg (get-user-arg username userid-p))
	 (res (post-json (concat url "/api/v1/users.resetAvatar")
			 (header auth-token)
			 (list arg))))
    (cdr (assoc :success (cl-json:decode-json-from-string res)))))

;; :TODO file load
;; research
;; Content-Type: multipart/form-data; boundary
@export
(defun users-set-avatar (url auth-token avatar-file
			 &optional (username nil) (userid-p nil) (avatar-url nil))
  )

@export
(defun users-update (url auth-token reg-info)
  (cl-json:decode-json-from-string
   (post-json (concat url "/api/v1/users.update") (header auth-token) (reg-info-to-alist reg-info))))
