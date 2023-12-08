package util

import (
	"io"
	"os"
)

// HandleInput reads all the data from the given path and returns it.
// if the path == '-' data from stdin is read. this is useful for quick prototyping and debugging
func HandleInput(path string) ([]byte, error) {
	if path == "-" {
		return io.ReadAll(os.Stdin)
	}

	return os.ReadFile(path)
}
