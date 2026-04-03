package main

import (
	"log"
	"os"
)

func main() {
	log.SetOutput(os.Stderr)
	log.SetFlags(log.Ltime | log.Lshortfile)

	server := NewServer()
	server.Run()
}
