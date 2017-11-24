#!/bin/bash
if [$# -eq 0]
then
	echo "No port specified, defaulting to 3000"
else
	java -cp bin Server $1
fi