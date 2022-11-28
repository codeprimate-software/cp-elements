#!/bin/bash
mvn -P code-analysis -Dcheckstyle.config.location=./etc/checkstyle/config/codeprimate_checks.xml \
 -Dcheckstyle.suppressions.file=./etc/checkstyle/config/suppressions.xml \
 -Dcheckstyle.suppressions.location=./etc/checkstyle/config/suppressions.xml \
 checkstyle:checkstyle
