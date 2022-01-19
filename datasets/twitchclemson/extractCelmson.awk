#!/usr/bin/awk -f

BEGIN { FS = "," }

NR > 1 {
    n = split($0,line,/[\t]/);

    gsub(/,/, "", line[4]);
    if (!( line[4] ~ /^\$\-.*/ ) && !( line[4] ~ /\$[0-9][0-9][0-9]+/)) {
	    print substr(line[4],2);
    }
}
