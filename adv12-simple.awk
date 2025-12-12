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
    for (i = 0; i <= current; i++)
        needed += $(4 + i) * x[i]
    if (actual >= needed)
        count++
}
END { print count }
