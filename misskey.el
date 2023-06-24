;;; misskey.el --- API library for misskey       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cj-bc a.k.a Cj.bc_sd

;; Author:  <cj.bc_sd@outlook.jp>
;; Keywords: comm, lisp
;; Package-Requires: (request-deferred deferred)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'deferred)
(require 'request-deferred)
(require 'cl-macs)
(require 'seq)
(require 'org-id) ; Used for generating session-id

;;;; Misskey environments
(cl-defstruct misskey/misskeyEnv
  "Contains configurations for API call."
  (host :type string)
  (token :type string)
  (username :type string))

(defvar misskey/envs `(:envs ,(make-hash-table :test 'equal) :default nil)
  "List of `misskey/misskeyEnv' and index of default env.
User can store envs as many as they want.
Default env is indicated by the index.")

(defun misskey/envs/empty ()
  "Return empty "
  `(:envs ,(make-hash-table :test 'equal) :default nil))

(defun misskey/envs/make-key (username host)
  "Returns appropriate key for given USERNAME and HOST."
  (format "%s@%s" username host))

(defun misskey/envs/get (username host)
  "Returns env for USERNAME@HOST if exists, nil otherwise."
  (gethash (misskey/envs/make-key username host) (plist-get misskey/envs :envs)))

(defun misskey/envs/set-default (username host)
  "Set default env to given one. If `misskey/envs' doesn't have env for given USRNAME@HOST,
do nothing and return nil."
  (when (misskey/envs/get username host)
    (plist-put misskey/envs :default (misskey/envs/make-key username host))))

(defun misskey/envs/add (host username token)
  "Add new env to `misskey/envs'."
  (interactive "sHost(e.g. misskey.io): \nsUsername: \nsToken: ")
  (let ((env (make-misskey/misskeyEnv :host host :token token :username username))
	(key (misskey/envs/make-key username host)))
    (puthash key env (plist-get misskey/envs :envs))
    (unless (plist-get misskey/envs :default)
      (plist-put misskey/envs :default key))))

(defun misskey/envs/remove (username host)
  "Remove selected env from `misskey/envs'"
  (interactive "sUsername: \nsHost: ")
  (when (misskey/envs/get username host)
    (remhash (misskey/envs/make-key username host) (plist-get misskey/envs :envs))))

(defun misskey/envs/get-default ()
  "Returns default env if at least one env is provided, nil otherwise."
  (when (plist-get misskey/envs :default)
    (gethash (plist-get misskey/envs :default) (plist-get misskey/envs :envs) )))

(defmacro misskey/with-default-env (env-sym body)
  "Set default misskeyEnv to ENV-SYM, warn if and execute BODY"
  `(let ((,env-sym (misskey/envs/get-default)))
     (if (null ,env-sym) (warn "You haven't set env yet. You should set at least one.")
       ,body)))

;;;; Internal functions

(defun misskey/json/walk (obj key func)
  "walk plist and apply FUNC for each occurrence of KEY.
It assumes OBJ is valid plist.


Idea behind this function is coming from pandoc library in Haskell.
Its document might helps you to understand concept of it:
https://hackage.haskell.org/package/pandoc-types-1.22.2.1/docs/Text-Pandoc-Walk.html


* Examples

```
(misskey/json/walk '(:id 0 :createdAt \"2022-01-01T12:30:10.00Z\"
			 :renote (:id 1 :createdAt  \"2022-01-01T12:30:10.00Z\"))
		   :createdAt '(lambda (d) (iso8601-parse d)))
>> (:id 0 :createdAt (10 30 12 1 1 2022 nil nil 0) :renote (:id 1 :createdAt (10 30 12 1 1 2022 nil nil 0)))
```
"
  (cond
   ((plistp obj)
    (when (plist-member obj key)
      (setq obj (plist-put (copy-sequence obj) key (funcall func (plist-get obj key)))))
    (seq-map `(lambda (item) (misskey/json/walk item ,key (function ,func))) obj))
   ((stringp obj) obj) ;; As string is 'sequence', I have to treat string before testing `sequencep'
   ((sequencep obj) (seq-map `(lambda (item) (misskey/json/walk item ,key (function ,func))) obj))
   (t obj)))

(defun misskey/json/walks (obj &rest keys)
  "`misskey/json/walk' wrapper to support multiple KEY FUNC pairs.

KEYS should be list, and elements are (KEY FUNC KEY2 FUNC2 KEY3 FUNC3...) for `misskey/json/walk'.
Therefore, length of KEYS should be multiple of 2. If not, it will return `nil'.

# Example:

(misskey/json/walks obj
  :foo a
  :bar b)

is equivalent to:

(misskey/json/walk
  (misskey/json/walk obj :foo a)
  :bar b)


# Real world example:

(misskey/json/walks obj
  :createdAt #'iso8601-parse
  :updatedAt #'iso8601-parse)

is equivalent to:

(misskey/json/walk
  (misskey/json/walk obj :createdAt #'iso8601-parse)
  #'iso8601-parse)
"
  (when (= (% (seq-length keys) 2) 0)
    (cl-loop for pair in (seq-partition keys 2)
  	  do (setq obj (misskey/json/walk obj (elt pair 0) (elt pair 1)))
  	  finally return obj)))

(defun misskey/json-read (&optional buf)
  "`json-read' with some modification for ease.

1. Convert timestamp string into Lisp timestamp"
  (with-current-buffer (or buf (current-buffer))
    (let* ((json-object-type 'plist)
	   (json-false :json-false)
	   (raw (json-read)))
      (misskey/json/walk (misskey/json/walk raw :createdAt #'iso8601-parse)
			 :updatedAt #'iso8601-parse))))

(defun misskey/debug/show-deferred-result (d)
  "[DEBUG-PURPOSE] print and display result of deffered-object D in new buffer.
It'll split frame to pop up that buffer

usage:
(misskey/debug/insert-deffered-result
 (misskey/api/users/show env '(:username \"cj_bc_sd\")))
→ will 
"
  (deferred:nextc d
    (lambda (result)
      (let ((buf (get-buffer-create
		  (generate-new-buffer-name "misskey/debug/show-deferred-result"))))
	(with-current-buffer buf
	  (emacs-lisp-mode)
	  (let ((print-length nil))
	    (insert (pp result))))
	(display-buffer buf 'display-buffer-in-side-window)))))

(defun misskey/call-deferred (env path body &optional credential-required)
  "General function to call api. This will return deferred object immediately, and run request asynchronously.

ENV should be `misskey/misskeyEnv' struct. PATH should be a string indicating relataive path of the API
endpoint(e.g. \"users/show\". BODY should be plist of valid request body for PATH."
  (request-deferred
   (format "https://%s/api/%s" (misskey/misskeyEnv-host env) path)
   :type "POST"
   :data (json-encode (if credential-required (plist-put body :i (misskey/misskeyEnv-token env)) body))
   :parser 'misskey/json-read
   :headers '(("Content-Type" . "application/json"))
   :error (cl-function (lambda (&key error-thrown data &allow-other-keys)
  			 (message "misskey/call-deferred: %s \n%s" error-thrown (plist-get (plist-get data :error) :message))))))

(cl-defmacro misskey-api (path &key credential required-params optional-params)
  "Create misskey/PATH function as misskey-api endpoint.
PATH is symbol of API endpoint path, such as users/show or users/notes.

If API endpoint requires authentication, set CREDENTIAL non-nil.

REQUIRED-PARAMS represents list of required parameters and their validators.
There're two way to specify parameters: simple list or list following `anyOf'.

If endpoint requires all of given params, use simple list. If endpoint requires
one or more of given params (which is represented as 'anyOf' in misskey spec),
use `anyOf' followed by list.

Either case, element of list are (PARAMETER-NAME . VALIDATOR).

OPTIONAL-PARAMS represents list of optional parameters and their validators.
Optional parameters are defined as \"keyword arguments\" by using `cl-defun'.
Unlike REQUIRED-PARAMS, it is simple list, and element of list are
(PARAMETER-NAME . VALIDATOR). PARAMETER-NAME is directly used as keyword argument,
 so _do not_ make it keyword (by start with \":\").
"
  (let* ((path-str (symbol-name path))
	 (name-sym (intern (format "misskey/api/%s" path-str)))

	 ;; If `:required-params' starts with `anyOf' symbol, I have to receive required parameters as
	 ;; simple list and append some code to validate arguments
	 (is-required-arg-anyOf (and (sequencep required-params) (eq (car required-params) 'anyOf)))
	 (required-args (cond ((null required-params) '())
			      (is-required-arg-anyOf (seq-map #'car (cdr required-params)))
			      (t (seq-map #'car required-params))))
	 (required-arg-valiator
	  (cond ((null required-params) '(t))
		(is-required-arg-anyOf `((or ,@(seq-map '(lambda (x) `(,(cdr x) ,(car x))) (cdr required-params)))))
		(t (seq-map '(lambda (x) `(,(cdr x) ,(car x))) required-params))))
	 (optional-args
	  (when optional-params (seq-map #'car optional-params)))
	 (optional-arg-validator
	  (if (null optional-args) '(t)
	    (seq-map '(lambda (x) `(or (null ,(car x)) (,(cdr x) ,(car x)))) optional-params)))
	 (request-body (when (or required-args optional-params)
			 (let ((keys (seq-map
				      '(lambda (name)
					 `(list ,(intern (format ":%s" (symbol-name name))) ,name))
				      `(,@required-args ,@optional-args))
				))
			 `(seq-reduce
			   (lambda (acc name)
			     (if (seq-elt name 1) `(,@acc ,@name) acc))
			   (list ,@keys) '())))))
    `(cl-defun ,name-sym (env ,@required-args ,@(when optional-args `(&key ,@optional-args)))
       ;; Even though I can remove 'when' clause technically when no argument is given,
       ;; I intentionally not to do so because that would make code more complicated.
       ;; As a result, generated code sometimes have something like:
       ;;
       ;; (when t t (deferred:$ ...))
       (when (and ,@required-arg-valiator ,@optional-arg-validator)
	 (deferred:$
	     (misskey/call-deferred env ,path-str ,request-body ,credential)
	     (deferred:nextc it 'request-response-data))))))


;;;; Predicates
(defun misskey-visibility-p (object)
  "Return t if OBJECT is valid string represents visibility.
Valid strings are:

- \"public\"
- \"home\"
- \"followers\"
- \"specified\"
"
  (and (stringp object)
   (or (string-equal object "public")
       (string-equal object "home")
       (string-equal object "followers")
       (string-equal object "specified"))))

(defun misskey-id-p (object)
  "Return t if OBJECT is valid misskey id.

Official: https://github.com/misskey-dev/misskey/blob/develop/packages/backend/src/models/id.ts
"
  (and (stringp object)
       (length= object 32)))

(defun misskey-sequence-of-id-p (object)
  "Return t if OBJECT is sequence of valid misskey id "
  (and (sequencep object)
       (seq-every-p 'misskey-id-p object)))

(defun misskey-decoded-time-p (object)
  "Return t if OBJECT is the same as result of `decode-time'."
  (and (sequencep object)
       (length= object 6)
       (let ((second (decoded-time-second object))
 	     (minutes (decoded-time-minute object))
 	     (hour (decoded-time-hour object))
 	     (day (decoded-time-day object))
 	     (month (decoded-time-month object))
 	     (year (decoded-time-year object))
 	     (dow (decoded-time-weekday object))
 	     (dst (decoded-time-dst object))
 	     (utcoff (decoded-time-zone object)))
 	 (and (or (and (constp second) (length= second 2) (seq-every-p second 'integerp))
	       (and (integerp second) (>= second 0) (<= second 60)))
 	      (integerp minutes) (>= minutes 0) (<= minutes 59)
 	      (integerp hour) (>= hour 0) (<= hour 23)
 	      (integerp day) (>= day 1) (<= day 31)
 	      (integerp month) (>= month 1) (<= month 12)
 	      ;; "YEAR" is written as 'typically greater than 1900' in
 	      ;; document, but I thought smaller values are still valid.
 	      (integerp year) (>= year 1)
 	      (integerp dow) (>= dow 0) (<= dow 6)
 	      (or (eq dst t) (eq dst nil) (eq dst -1))
 	      (integerp utcoff)))))


(defun misskey-permission-p (object)
  "Return t if OBJECT is valid permission string.

Official Reference: https://misskey-hub.net/docs/api/permission.html

Valid strings are:
- \"read:account\"
- \"write:account\"
- \"read:blocks\"
- \"write:blocks\"
- \"read:drive\"
- \"write:drive\"
- \"read:favorites\"
- \"write:favorites\"
- \"read:following\"
- \"write:following\"
- \"read:messaging\"
- \"write:messaging\"
- \"read:mutes\"
- \"write:mutes\"
- \"write:notes\"
- \"read:notifications\"
- \"write:notifications\"
- \"write:reactions\"
- \"write:votes\"
- \"read:pages\"
- \"write:pages\"
- \"write:page-likes\"
- \"read:page-likes\"
- \"write:gallery-likes\"
- \"read:gallery-likes\"
"
  (and (stringp object)
       (or (string-equal object "read:account")
	   (string-equal object "write:account")
	   (string-equal object "read:blocks")
	   (string-equal object "write:blocks")
	   (string-equal object "read:drive")
	   (string-equal object "write:drive")
	   (string-equal object "read:favorites")
	   (string-equal object "write:favorites")
	   (string-equal object "read:following")
	   (string-equal object "write:following")
	   (string-equal object "read:messaging")
	   (string-equal object "write:messaging")
	   (string-equal object "read:mutes")
	   (string-equal object "write:mutes")
	   (string-equal object "write:notes")
	   (string-equal object "read:notifications")
	   (string-equal object "write:notifications")
	   (string-equal object "write:reactions")
	   (string-equal object "write:votes")
	   (string-equal object "read:pages")
	   (string-equal object "write:pages")
	   (string-equal object "write:page-likes")
	   (string-equal object "read:page-likes")
	   (string-equal object "write:gallery-likes")
	   (string-equal object "read:gallery-likes"))))



;;;; API caller functions
;;;;; miauth
(cl-defun misskey/api/miauth/initiate (host &key name icon-url permissions)
  "One of endpoint for Miauth. This will generate sessionID and returns it along with URL user should access.
In order to accomplish miauth, you should call `misskey/api/miauth/check' with given sessionID after user confirmed
application access."
  (let* ((sessionId (org-id-uuid))
	(nameQ (when name (format "name=%s" name)))
	(iconQ (when icon-url (format "icon=%s" icon-url)))
	(permissionsQ (when permissions (format "permission=%s" (string-join permissions ","))))
	(queries (string-join `("?" ,(string-join (seq-filter #'identity (list nameQ iconQ permissionsQ)) "&")))))
    (list :url (format "https://%s/miauth/%s%s" host sessionId queries)
	  :session-id sessionId)))

(defun misskey/api/miauth/check (host session-id &rest timeout)
  "Check miauth and returns plist of result.

Please call `misskey/api/miauth/initiate' before call this, and retrive SESSION-ID.
HOST should be instance HOST(e.g. \"misskey.io\".)
SESSION-ID should be one that is returned from `misskey/api/miauth/initiate'.

Result will be plist of two keys if miauth succeeded:
+ `:token' contains actual token to use
+ `:user' contains user information of that token.

If miauth failed(i.e. \"{ok: false}\"), this function will return nil.
If response is invalid, this function will throw error.
"
  ;; I'm sure *it is highly discouraged to use sync*, but I have no choice
  ;; as I need to receive and return value.
  (let ((result (request-response-data
		 (request (format "https://%s/api/miauth/%s/check" host session-id)
		   :type "POST"
		   :parser 'misskey/json-read
		   :sync t
		   :timeout (or timeout 3)))))
    (pcase (plist-get result :ok)
      ('t `(:token ,(plist-get result :token) :user ,(plist-get result :user)))
      (:json-false nil)
      (_ (error "misskey/api/miauth/check: Invalid miauth response")))))

;;;;; Normal APIs
(misskey-api users/show
	     :credential nil
	     :required-params
	     (anyOf (userId . stringp)
		    (userIds . misskey-sequence-of-id-p)
		    (username . stringp)))

(misskey-api notes/local-timeline :credential nil)
(misskey-api notes/create :credential t
	     :required-params
	     (anyOf
	      (text . stringp)
	      (fileIds . misskey-sequence-of-id-p)
	      (mediaIds . misskey-sequence-of-id-p)
	      (renoteId . stringp))
	     :optional-params
	     ((visibility . misskey-visibility-p)
	      (visibleUserIds . misskey-sequence-of-id-p)
	      (cw . stringp) (localOnly . booleanp) (noExtractMentions . booleanp)
	      (noExtractHashtags . booleanp) (noExtractEmojis . booleanp)
	      (replyId . stringp) (channelId . stringp)))

(misskey-api users/notes
	     :credential nil
	     :required-params
	     ((userId . stringp))
	     :optional-params
	     ((includeReplies . booleanp)
	      (limit . integerp)))

(misskey-api notes/children :credential nil
	     :required-params
	     ((noteId . misskey-id-p))
	     :optional-params
	     ((limit . integerp)
	      (sinceId . misskey-id-p)
	      (untilId . misskey-id-p)))

(misskey-api notes/conversation :credential nil
	     :required-params
	     ((noteId . misskey-id-p))
	     :optional-params
	     ((limit . integerp)
	      (offset . integerp)))

(misskey-api notes/delete :credential t
	     :required-params ((noteId . misskey-id-p)))

(misskey-api notes/favorites/create :credential t
	     :required-params ((noteId . misskey-id-p)))

(misskey-api notes/favorites/delete :credential t
	     :required-params ((noteId . misskey-id-p)))

(misskey-api notes/featured :credential nil
	     :optional-params
	     ((limit . integerp)
	      (offset . integerp)))
(misskey-api notes/global-timeline :credential nil
	     :optional-params
	     ((withFiles . booleanp)
	      (limit . integerp)
	      (sinceId . misskey-id-p) (untilId . misskey-id-p)
	      (sinceDate . integerp) (untilDate . integerp)))


(misskey-api notes/timeline :credential t
	     :optional-params
	     ((limit . integerp) (sinceId . stringp) (untilId . stringp)
	      (sinceData . misskey-decoded-time-p) (untilData . misskey-decoded-time-p)
	      (includeMyRenotes . booleanp) (includeMyRenotedMyNotes . booleanp) (includeLocalRenotes . booleanp)
	      (withFiles . booleanp)))



(provide 'misskey)
;;; misskey.el ends here

;; Local Variables:
;; outline-regexp: ";;;\\(;*\\) \\(?:\\sw\\|\\s-\\)+"
;; End:
