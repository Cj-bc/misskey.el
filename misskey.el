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
(require 'subr)
(require 'seq)

(cl-defstruct misskey/misskeyEnv
  "Contains configurations for API call."
  (host :type string)
  (token :type string))


(defun misskey/call-deferred (env path body)
  "General function to call api. This will return deferred object immediately, and run request asynchronously.

ENV should be `misskey/misskeyEnv' struct. PATH should be a string indicating relataive path of the API
endpoint(e.g. \"users/show\". BODY should be plist of valid request body for PATH."
  (request-deferred
   (format "https://%s/api/%s" (misskey/misskeyEnv-host env) path)
   :type "POST"
   :data (json-encode body)
   :parser '(lambda ()
  	      (let ((json-object-type 'plist))
  		(json-read)))
   :headers '(("Content-Type" . "application/json"))
   :error (cl-function (lambda (&key error-thrown &allow-other-keys)
  			 (message "failed to retrive info. (%s)" error-thrown)))
   :success (cl-function (lambda (&key data &allow-other-keys)
  			   (with-current-buffer (get-buffer-create "*scratch*") data)))))

(cl-defstruct misskeyEndpoint
  (path :type string)
  (request-validator :type function))


(defun misskey/api/users/show/--validate-request (body)
  "Return t when body is valid, `nil' otherwise.
This allows other keys existence (Won't return nil even if there's some keys that aren't recognized)

Valid bodies are:
- (:userId \"USERID\")
- (:userIds (\"USERID\" \"USERID2\" ...))
- (:username \"USERNAME\")
- (:username \"USERNAME\" :host \"HOSTNAME\")
"
  (or (stringp (plist-get body :userId))
      (and (sequencep (plist-get body :userIds))
	   (seq-every-p 'stringp (plist-get body :userIds)))
      (and (stringp (plist-get body :username))
	   (string-or-null-p (plist-get body :host)))))

(defun misskey/api/users/show (env body)
  "call users/show API. It'll return deferred object when succeed to
call API properly, nil otherwise.

https://misskey-hub.net/docs/api/endpoints/users/show.html"
  (when (misskey/api/users/show/--validate-request body)
    (deferred:$
      (misskey/call-deferred env "users/show" body)
      (deferred:nextc it 'request-response-data))))

(provide 'misskey)
;;; misskey.el ends here
