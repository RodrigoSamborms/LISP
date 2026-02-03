#!/bin/bash
# Script interactivo para prueba del caballo
cat <<EOF | rlwrap sbcl
(load "/mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/sistema_experto_V6.lisp")
(sistema-experto)
EOF
