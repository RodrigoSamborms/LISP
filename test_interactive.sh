#!/bin/bash
# Script para prueba interactiva del caballo en SBCL
cat << 'EOF' | wsl bash -c "cd /mnt/c/Users/sambo/Documents/GitHub/LISP/sistemaExperto_Ejemplo/versiones/ && rlwrap sbcl"
(load "sistema_experto_V6.lisp")
(sistema-experto)
EOF
