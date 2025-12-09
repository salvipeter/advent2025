BEGIN { FS = "," }
{
    print "v", $1 / 1000.0, $2 / 1000.0, 0
}
END {
    line = "l"
    for (i = 1; i < NR; i++)
        line = line " " i
    print line, 1
}
