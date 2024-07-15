#!/bin/sh
echo "Usage: $0 input_xml output_ada"
./dbus_binding_generator_ada "$1" > "$2" && gnatpp "$2" && gnatchop -w "$2" "$(dirname $2)"
