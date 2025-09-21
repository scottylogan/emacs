#! /bin/bash

cd "$(dirname "${0}")" || exit

CONFIG="${HOME}/.config"
here=$(pwd -P)

link="/bin/ln -sfh"
[[ -f /etc/os-release ]] && link="ln -sfn"

# make ~/.config if it doesn't exist
[ -d "${CONFIG}" ] || mkdir "${CONFIG}"

${link} "${here}" "${CONFIG}/emacs"
