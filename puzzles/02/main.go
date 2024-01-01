package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"

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

	fmt.Println("part1:", Part1(data))
	fmt.Println("part2:", Part2(data))
}

func Part1(data []byte) int {
	lines := strings.Split(string(data), "\n")

	var result int

	constraint := Play{Red: 12, Green: 13, Blue: 14}

	for _, l := range lines {
		if l == "" {
			continue
		}

		g, err := parseGame(l)

		if err != nil {
			panic(err)
		}

		if isGamePossible(g, constraint) {
			result += int(g.Game)
		}
	}

	return result
}

func Part2(data []byte) int {
	lines := strings.Split(string(data), "\n")

	var result int

	for _, l := range lines {
		if l == "" {
			continue
		}

		g, err := parseGame(l)

		if err != nil {
			panic(err)
		}

    req := getMinimalRequired(g)

    pow := req.Blue * req.Green * req.Red
    result += pow
	}

	return result
}

func getMinimalRequired(g Game) Play {
	var result Play

	for _, p := range g.Plays {
		result.Blue = max(result.Blue, p.Blue)
		result.Green = max(result.Green, p.Green)
		result.Red = max(result.Red, p.Red)
	}

	return result
}

func max(a, b int) int {
	if a > b {
		return a
	}

	return b
}

func isGamePossible(g Game, constraints Play) bool {
	for _, p := range g.Plays {
		if p.Blue > constraints.Blue || p.Red > constraints.Red || p.Green > constraints.Green {
			return false
		}

	}

	return true
}

type Game struct {
	Game  uint64
	Plays []Play
}

type Play struct {
	Red, Blue, Green int
}

func parseGame(line string) (Game, error) {
	splits := strings.Split(line, ":")

	game := splits[0]
	rawPlays := strings.Split(splits[1], ";")

	gameNumber, err := strconv.ParseUint(strings.Split(game, " ")[1], 10, 64)

	if err != nil {
		return Game{}, err
	}

	var plays []Play

	for _, p := range rawPlays {
		p, err := parsePlay(p)

		if err != nil {
			return Game{}, err
		}

		plays = append(plays, p)
	}

	return Game{
		Game:  gameNumber,
		Plays: plays,
	}, nil
}

var playRegex = regexp.MustCompile(`(\d+) (blue|red|green)`)

func parsePlay(play string) (Play, error) {
	p := Play{}

	matches := playRegex.FindAllStringSubmatch(play, -1)

	for _, m := range matches {
		count, err := strconv.Atoi(m[1])

		if err != nil {
			return Play{}, err
		}

		switch m[2] {
		case "blue":
			p.Blue = count
		case "red":
			p.Red = count
		case "green":
			p.Green = count
		}
	}

	return p, nil
}
