package main

import (
	"bufio"
	"strings"
	"testing"
)

// A Content-Length that isn't a number used to be read as 0, leaving the
// body in the stream to be parsed as the next header.
func TestReadMessageRejectsNonNumericLength(t *testing.T) {
	r := bufio.NewReader(strings.NewReader("Content-Length: abc\r\n\r\n{}"))
	if _, err := readMessage(r); err == nil {
		t.Fatal("expected an error for a non-numeric Content-Length")
	}
}

// A huge Content-Length used to be allocated up front: the server died
// with "fatal error: runtime: out of memory".
func TestReadMessageRejectsHugeLength(t *testing.T) {
	r := bufio.NewReader(strings.NewReader("Content-Length: 99999999999999\r\n\r\n{}"))
	if _, err := readMessage(r); err == nil {
		t.Fatal("expected an error for an oversized Content-Length")
	}
}

func TestReadMessageRejectsNegativeLength(t *testing.T) {
	r := bufio.NewReader(strings.NewReader("Content-Length: -5\r\n\r\n{}"))
	if _, err := readMessage(r); err == nil {
		t.Fatal("expected an error for a negative Content-Length")
	}
}

func TestReadMessageReadsAWellFormedMessage(t *testing.T) {
	r := bufio.NewReader(strings.NewReader("Content-Length: 2\r\n\r\n{}"))
	body, err := readMessage(r)
	if err != nil || string(body) != "{}" {
		t.Fatalf("got %q, %v", body, err)
	}
}
