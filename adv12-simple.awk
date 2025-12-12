# Run with: awk -f adv12-simple.awk adv12.txt
BEGIN { FS="[x: ]" }
/^[0-9]:$/ { current = $1 }
/#/ {
    split($0, s, "")
    for (i = 1; i <= length($0); i++)
        if (s[i] == "#")
            x[current]++
}
/x/ {
    actual = $1 * $2
    needed = 0
    blocks = int($1 / 3) * int($2 / 3)
    for (i = 0; i <= current; i++) {
        needed += $(4 + i) * x[i]
        blocks -= $(4 + i)
    }
    if (actual >= needed) {
        if (blocks < 0)
            print "Ambiguous: ", $0
        count++
    }
}
END { print count }
