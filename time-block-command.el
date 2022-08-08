;;; time-block-command.el --- Block running commands using time -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://git.sr.ht/~swflint/time-block-command
;; Version: 0.2.1
;; Package-Requires: ((time-block "0.2.1"))
;; Keywords: tools, productivity, convenience

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
;; This file loads `time-block' to provide compatibility with the old
;; `time-block-command' package.

(require 'time-block)

(warn "Package `time-block-command' should be loaded as `time-block'.")

;;; Code:

(provide 'time-block-command)

;;; time-block-command.el ends here
