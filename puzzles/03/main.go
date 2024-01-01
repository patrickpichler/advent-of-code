package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/patrickpichler/advent-of-code/pkgs/util"
)

func main() {
	input := "puzzles/03/input"

	if len(os.Args) > 1 {
		input = os.Args[1]
	}

	data, err := util.HandleInput(input)

	if err != nil {
		panic(err)
	}

	fmt.Println("part1:", Part1(data))
	fmt.Println("part2:", Part2(data))
}

func Part1(rawData []byte) any {
	data := string(rawData)

	lines := strings.Split(data, "\n")

	visited := map[Position]bool{}
	var result int

	for r, l := range lines {
		for c, b := range l {
			if isSymbol(b) {
				nums := extractAdjacentNumbers(c, r, lines, visited)

				for _, n := range nums {
					result += n
				}
			}
		}
	}

	return result
}

func Part2(rawData []byte) any {
	data := string(rawData)

	lines := strings.Split(data, "\n")

	visited := map[Position]bool{}
	var result int

	for r, l := range lines {
		for c, b := range l {
			if b == '*' {
				nums := extractAdjacentNumbers(c, r, lines, visited)

				if len(nums) != 2 {
					continue
				}

				result += nums[0] * nums[1]
			}
		}
	}

	return result
}

type Position struct {
	X, Y int
}

var adjacentPositionTransformations []Position = []Position{
	{-1, -1},
	{-1, 0},
	{-1, 1},
	{0, -1},
	{0, 0},
	{0, 1},
	{1, -1},
	{1, 0},
	{1, 1},
}

func extractAdjacentNumbers(col, row int, lines []string, visited map[Position]bool) []int {
	var result []int

	for _, t := range adjacentPositionTransformations {
		x := col + t.X
		y := row + t.Y

		// this works because all lines have the same length
		if y < 0 || y > len(lines) || x < 0 || x > len(lines[0]) {
			continue
		}

		pos := Position{X: x, Y: y}

		// number was already extracted
		if visited[pos] {
			continue
		}

		found, num, start, end := extractNumber(x, []rune(lines[y]))

		visited[pos] = true

		if !found {
			continue
		}

		for i := start; i <= end; i++ {
			visited[Position{X: i, Y: pos.Y}] = true
		}

		result = append(result, num)
	}

	return result
}

func extractNumber(col int, line []rune) (bool, int, int, int) {
	if col < 0 || col >= len(line) || !isNumber(line[col]) {
		return false, 0, 0, 0
	}

	start := col
	end := col
	cur := col
	num := -1

	for {
		cur -= 1

		if cur < 0 || cur >= len(line) || !isNumber(line[cur]) {
			break
		}

		start = cur
	}

	cur = end

	for {
		cur += 1

		if cur < 0 || cur >= len(line) || !isNumber(line[cur]) {
			break
		}

		end = cur
	}

	numStr := string(line[start:min(len(line), end+1)])

	num, err := strconv.Atoi(numStr)

	if err != nil {
		// BUG nothing we can do about
		panic(err)
	}

	if num < 0 {
		return false, 0, 0, 0
	}

	return true, num, start, end
}

func min(a, b int) int {
	if a > b {
		return b
	}

	return a
}

func isSymbol(r rune) bool {
	return ('0' > r || r > '9') && r != '.'
}

func isNumber(r rune) bool {
	return '0' <= r && r <= '9'
}
