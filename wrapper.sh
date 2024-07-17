#!/bin/sh
("$(dirname $0)"/dbus_binding_generator_ada "$1" client > "$2" && gnatchop -w "$2" "$(dirname $2)" && gnatpp "$(dirname $2)"/*.ads) || echo "Usage: $0 input_xml output_ada"
