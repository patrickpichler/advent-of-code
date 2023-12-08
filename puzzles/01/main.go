package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"

	"github.com/patrickpichler/advent-of-code/pkgs/util"
)

func main() {
	input := "puzzles/01/input"

	if len(os.Args) > 1 {
		input = os.Args[1]
	}

  data, err := util.HandleInput(input)

  if err != nil {
    panic(err)
  }

	println("part1:", Part1(data))
	println("part2:", Part2(data))
}

func Part1(data []byte) int {
	var first, last byte
	var sum int

	// this is used just for debugging
	var lineNumber int

	for _, b := range data {
		if b == '\n' {
			if first == 0 || last == 0 {
				// something weng wrong and we didn't detect any numbers
				panic(fmt.Sprint("didn't detect number for line", lineNumber))
			}

			// we need to calculate the sum for each line
			num := int(first)*10 + int(last)
			sum += num
			first = 0
			last = 0
			lineNumber++
			continue
		}

		if b < byte('0') || b > byte('9') {
			// this means we didn't get a number char ==> ignore
			continue
		}

		num := b - byte('0')

		last = num

		if first == 0 {
			first = num
		}
	}

	return sum
}

func Part2(data []byte) int {
	scanner := bufio.NewScanner(bytes.NewReader(data))

	var sum int

	for scanner.Scan() {
		numbers := extractNumbers(scanner.Text())
		sum += int(numbers[0]*10 + numbers[len(numbers)-1])
	}

	return sum
}

var numbers = []string{
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
}

func extractNumbers(line string) []byte {
	var result []byte

	for pos, char := range line {
		b := byte(char)

		if b >= '0' && b <= '9' {
			result = append(result, b-'0')
			continue
		}

		for numValue, numberString := range numbers {
			if len(line) < len(numberString)+pos {
				continue
			}

			if line[pos:pos+len(numberString)] == numberString {
				result = append(result, byte(numValue)+1)
				break
			}
		}
	}

	return result
}
