;;; time-block-command.el --- Block running emacs commands using time -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://git.sr.ht/~swflint/time-block-command
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.0") (ts "0.1"))
;; Keywords: productivity, time blocking

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

(require 'ts)

;;; Code:

;; Variables:

(defvar time-block-command-groups nil
  "Groups used for blocking commands by time.

Alist, key is group name (symbol) and alist of blocks: (day
. ((start end) ...)), with day as numeric day of week.")

(defun time-block-group-blocked-p (group-name)
  "Is group GROUP-NAME currently blocked?"
  (when-let ((group (rest (assoc group-name time-block-command-groups)))
             (ts-now (ts-now))
             (current-day (ts-dow ts-now))
             (day-blocks (rest (assoc current-day group)))
             (now (ts-parse (ts-format "%H:%M" ts-now))))
    (do* ((pair (first day-blocks) (first blocks-left))
          (blocks-left (rest day-blocks) (rest blocks-left))
          (start (ts-parse (car pair)) (ts-parse (car pair)))
          (end (ts-parse (cdr pair)) (ts-parse (cdr pair))))
        ((or (null blocks-left)
             (and (ts<= start now)
                  (ts<= now end)))
         (and (ts<= start now)
              (ts<= now end))))))

(cl-defmacro timeblock-define-block-command (name argslist (group block-message &optional override-prompt) &body body)
  "Define NAME as a time-blocked command.

ARGSLIST are the arguments which the command takes.
DOCSTRING is the documentation string and is optional.

GROUP is the `time-block-command-groups' to use to determine
whether or not to run.

BLOCK-MESSAGE is the message to show when run is blocked.

If OVERRIDE-PROMPT is present, then ask if blocking should be
overriden.

BODY is the body of the code.  This should include an
`interactive' specification matching \\=ARGSLIST.

\(fn NAME ARGSLIST (GROUP BLOCK-MESSAGE [OVERRIDE-PROMPT]) [DOCSTRING] [INTERACTIVE-SPEC] [BODY...])"
  (declare (debug (&define name lambda-list
                           sexp
                           [&optional lambda-doc]
                           [&optional ("interactive" interactive) def-body]))
           (indent 3)
           (doc-string 4))
  (let* ((docstring (when (stringp (first body)) (first body)))
         (body (if docstring (rest body) body))
         (interactive-spec (when (and (listp (first body))
                                      (equal 'interactive (first (first body))))
                             (first body)))
         (body (if interactive-spec (rest body) body))
         (condition (if override-prompt
                        `(or (time-block-group-blocked-p ',group)
                             (yes-or-no-p ,override-prompt))
                      `(time-block-group-blocked-p ',group))))
    (if docstring
        `(defun ,name ,argslist
           ,docstring
           ,interactive-spec
           (if ,condition
               (message ,block-message)
             ,@body))
      `(defun ,name ,argslist
         ,interactive-spec
         (if ,condition
             (message ,block-message)
           ,@body)))))

(provide 'time-block-command)

;;; time-block-command.el ends here
