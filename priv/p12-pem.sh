#!/bin/sh
#
# FILE: p12-pem.sh
# USAGE:
#   ./p12-pem.sh <input_p12_file> [<output_pem_file>]
#

INPUT=
OUTPUT=
if [ $# -eq 1 ]; then
	INPUT=$1
	OUTPUT=$1".pem"
elif [ $# -eq 2 ]; then
	INPUT=$1
	OUTPUT=$2
else
	echo "USAGE:"
	echo "  "$0 "<input_p12_file> [<output_pem_file>]"
	echo ""
	exit 2
fi

TEMP1=$OUTPUT".tem1"
TEMP2=$OUTPUT".tem2"
TEMP3=$OUTPUT".tem3"
TEMP4=$OUTPUT".tem4"

openssl pkcs12 -in "$INPUT" -out "$TEMP1" -nodes
if [ $? -ne 0 ]; then
	echo ""
	echo ERROR: bad call of openssl pkcs12.
	rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4
	exit 1
fi
openssl x509 -inform pem -in "$TEMP1" -outform pem -out "$TEMP2"
if [ $? -ne 0 ]; then
	echo ""
	echo ERROR: bad call of openssl x509.
	rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4
	exit 1
fi
openssl rsa -inform pem -in "$TEMP1" -outform pem -out "$TEMP3"
if [ $? -ne 0 ]; then
	echo ""
	echo ERROR: bad call of openssl rsa.
	rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4
	exit 1
fi
cat "$TEMP2" "$TEMP3" > "$OUTPUT"
if [ $? -ne 0 ]; then
	echo ""
	echo ERROR: bad call of cat.
	rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4
	exit 1
fi

echo ""
echo Done with "'$OUTPUT'".
rm -f $TEMP1 $TEMP2 $TEMP3 $TEMP4

