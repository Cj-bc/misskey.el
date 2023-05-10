;;; misskey.el --- API library for misskey       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Cj-bc a.k.a Cj.bc_sd

;; Author:  <cj.bc_sd@outlook.jp>
;; Keywords: hypermedia
;; Package-Requires: (request-deferred)

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

(require 'request-deferred)
(require 'subr)
(require 'seq)

(defun misskey/call (host path body)
  "General function to call api"
  (request (format "https://%s/api/%s" host path)
	   :type "POST"
	   :data (json-encode body)
	   :parser '(lambda ()
		      (let ((json-object-type 'plist))
			(json-read)))
	   :headers '(("Content-Type" . "application/json"))
	   :error (cl-function (lambda (&key error-thrown &allow-other-keys)
		     (message "failed to retrive info. (%s)" error-thrown)))
	   :success (cl-function (lambda (&key data &allow-other-keys)
		       (with-current-buffer (get-buffer-create "*scratch*") data)))
	   ))

(cl-defstruct misskeyEndpoint
  (path :type string)
  (request-validator :type function))

(defun misskey/api/users/show/--validate-request (body)
  "Return t when body is valid, `nil' otherwise.

Valid body are:
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

(defun misskey/api/users/show (body)
  "call users/show API.

https://misskey-hub.net/docs/api/endpoints/users/show.html"
  (when (misskey/api/users/show/--validate-request body)
      (misskey/call "misskey.io" "users/show" body)))


(provide 'misskey)
;;; misskey.el ends here
