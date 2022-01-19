#!/usr/bin/awk -f

BEGIN {
    FS = ",";
    for (i=0;i<101;i++) {
	count[i] = 0;
    }
}

NR > 1 {
    if (NR < 1002) {
	count[$5]=count[$5]+1;
    }
}

END {
    for (i=0;i<101;i++) {
	print i "," count[i];
    }
}
