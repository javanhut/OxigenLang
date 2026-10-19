package main

import "unicode/utf8"

// LSP positions count UTF-16 code units, not bytes; these convert between
// the two within one line.

// byteOffset is the byte offset in line of UTF-16 column char, clamped to
// the line's length.
func byteOffset(line string, char uint32) int {
	units := uint32(0)
	for i, r := range line {
		if units >= char {
			return i
		}
		units += utf16Units(r)
	}
	return len(line)
}

// utf16Len is how many UTF-16 code units s takes.
func utf16Len(s string) uint32 {
	units := uint32(0)
	for _, r := range s {
		units += utf16Units(r)
	}
	return units
}

func utf16Units(r rune) uint32 {
	if r >= 0x10000 && r <= utf8.MaxRune {
		return 2
	}
	return 1
}
