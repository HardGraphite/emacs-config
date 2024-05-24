#!/bin/sh

if [ -z "${EMACS}" ]; then
    EMACS=emacs
fi

Q_INIT_DIR="${TMPDIR:-/tmp}/EmacsQ.${USER}"
Q_LISP_DIR="${Q_INIT_DIR}/lisp"
Q_INIT_FILE="${Q_INIT_DIR}/init.el"
Q_EXIT_RESTART=100

[ -d "${Q_INIT_DIR}" ] || mkdir -p "${Q_INIT_DIR}"
[ -d "${Q_LISP_DIR}" ] || mkdir "${Q_LISP_DIR}"
cat <<EOF > "${Q_INIT_FILE}"
;; init.el -*- lexical-binding: t -*-

(defun restart-emacs () (interactive) (kill-emacs ${Q_EXIT_RESTART}))
(global-set-key (kbd "<f4>") 'kill-emacs)
(global-set-key (kbd "<f5>") 'restart-emacs)

;;;

EOF

cd "${Q_INIT_DIR}"
echo "Starting Emacs at ${Q_INIT_DIR} ..."
while \
    "${EMACS}" \
        -Q --init-directory "${Q_INIT_DIR}" --load "${Q_INIT_FILE}" \
        --directory "${Q_LISP_DIR}" \
        --debug-init \
        --eval "(progn (find-file-other-window \"${Q_INIT_FILE}\") (goto-char (point-max)))"; \
    [ $? -eq ${Q_EXIT_RESTART} ]; \
do
    echo 'Restarting Emacs...'
done
