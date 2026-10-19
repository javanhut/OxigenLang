package main

import "testing"

// LSP positions count UTF-16 code units; the server indexed lines by byte,
// so anything after a non-ASCII character on the line was off.
func TestWordAtPositionCountsUTF16Units(t *testing.T) {
	src := "éé ab"
	// "ab" starts at UTF-16 column 3 (each é is one unit, two bytes).
	if got := getWordAtPosition(src, Position{Line: 0, Character: 3}); got != "ab" {
		t.Fatalf("got %q, want ab", got)
	}
}

func TestWordAtPositionAfterAnAstralCharacter(t *testing.T) {
	src := "😀 cd"
	// 😀 is two UTF-16 units and four bytes; "cd" starts at column 3.
	if got := getWordAtPosition(src, Position{Line: 0, Character: 3}); got != "cd" {
		t.Fatalf("got %q, want cd", got)
	}
}

func TestPositionToOffsetCountsUTF16Units(t *testing.T) {
	src := "a\n\"é\" + z"
	// Line 1, column 6 is "z" (byte 7 of the line).
	off := positionToOffset(src, Position{Line: 1, Character: 6})
	if src[off:] != "z" {
		t.Fatalf("offset %d points at %q, want \"z\"", off, src[off:])
	}
}
