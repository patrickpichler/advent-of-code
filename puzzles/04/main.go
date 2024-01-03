package main

import (
	"fmt"
	"os"
	"regexp"
	"slices"
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

	cards := parseInput(data)

	fmt.Println("part1:", Part1(cards))
	fmt.Println("part2:", Part2(cards))
}

type Card struct {
	Num            int
	WinningNumbers []int
	Numbers        []int
}

func Part1(cards []Card) any {
	var score int

	for _, c := range cards {
		score += scoreCard(c)
	}

	return score
}

func scoreCard(card Card) int {
	var score int

	for _, n := range card.Numbers {
		if slices.Contains(card.WinningNumbers[:], n) {
			score = max(score*2, 1)
		}
	}

	return score
}

func max(a, b int) int {
	if a > b {
		return a
	}

	return b

}

func Part2(cards []Card) any {
	counts := map[int]int{}

	cardCount := map[int]int{}

	result := 0

	for idx := len(cards) - 1; idx >= 0; idx-- {
		cur := cards[idx]

		matches := cardMatches(cur)

		numCards := 1

		for i := 0; i < matches; i++ {
			numCards += counts[cur.Num+i+1]
		}

		cardCount[cur.Num]++

		counts[cur.Num] = numCards
		result += numCards
	}

	return result
}

func cardMatches(card Card) int {
	var matches int

	for _, n := range card.Numbers {
		if slices.Contains(card.WinningNumbers[:], n) {
			matches += 1
		}
	}

	return matches
}

func parseInput(data []byte) []Card {
	var cards []Card

	for _, l := range strings.Split(string(data), "\n") {
		if l == "" {
			continue
		}

		cards = append(cards, parseLine(l))
	}

	return cards
}

var numberOrSeparator = regexp.MustCompile(`[\d\|]+`)

func parseLine(line string) Card {
	parts := numberOrSeparator.FindAllString(line, -1)

	cardId, err := strconv.Atoi(parts[0])

	if err != nil {
		// BUG nothing we can do about it
		panic(err)
	}

	var winningNumbers []int
	var numbers []int
	separatorFound := false

	for _, v := range parts[1:] {
		if v == "|" {
			separatorFound = true
			continue
		}

		num, err := strconv.Atoi(v)

		if err != nil {
			// BUG nothing we can do about it
			panic(err)
		}

		if separatorFound {
			numbers = append(numbers, num)
		} else {
			winningNumbers = append(winningNumbers, num)
		}
	}

	return Card{
		Num:            cardId,
		WinningNumbers: winningNumbers,
		Numbers:        numbers,
	}
}

func toNumers(matches []string) []int {
	result := make([]int, len(matches))

	for i, v := range matches {
		num, err := strconv.Atoi(v)

		if err != nil {
			// BUG nothing we can handle
			panic(err)
		}

		result[i] = num
	}

	return result
}
