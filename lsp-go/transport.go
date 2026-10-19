package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// maxMessageSize bounds a message body. The length comes from the client,
// and allocating whatever it claims let one bad header kill the server.
const maxMessageSize = 64 << 20

func readMessage(reader *bufio.Reader) ([]byte, error) {
	contentLength := -1
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			return nil, err
		}
		line = strings.TrimSpace(line)
		if line == "" {
			break
		}
		if after, ok := strings.CutPrefix(line, "Content-Length:"); ok {
			val := strings.TrimSpace(after)
			n, err := strconv.Atoi(val)
			// Without a usable length the body can't be found in the stream.
			if err != nil || n < 0 {
				return nil, fmt.Errorf("invalid Content-Length %q", val)
			}
			if n > maxMessageSize {
				return nil, fmt.Errorf("Content-Length %d exceeds the %d byte limit", n, maxMessageSize)
			}
			contentLength = n
		}
	}
	if contentLength < 0 {
		return nil, fmt.Errorf("missing Content-Length header")
	}
	body := make([]byte, contentLength)
	_, err := io.ReadFull(reader, body)
	return body, err
}

func writeMessage(writer io.Writer, body []byte) error {
	header := fmt.Sprintf("Content-Length: %d\r\n\r\n", len(body))
	if _, err := io.WriteString(writer, header); err != nil {
		return err
	}
	_, err := writer.Write(body)
	return err
}
