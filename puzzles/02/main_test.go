package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestIsGamePossible(t *testing.T) {
	type testCase struct {
		name       string
		g          Game
		constraint Play
		want       bool
	}

	testCases := []testCase{
		{
			name: "simple possible",
			g:    Game{Plays: []Play{{1, 1, 1}}},
			constraint: Play{
				Red:   100,
				Blue:  100,
				Green: 100,
			},
			want: true,
		},
		{
			name: "edge case, but possible",
			g:    Game{Plays: []Play{{99, 100, 100}, {1, 0, 0}}},
			constraint: Play{
				Red:   100,
				Blue:  100,
				Green: 100,
			},
			want: true,
		},
		{
			name: "edge case, but impossible",
			g:    Game{Plays: []Play{{1, 101, 1}, {0, 1, 0}}},
			constraint: Play{
				Red:   100,
				Blue:  100,
				Green: 100,
			},
			want: false,
		},
	}

	for _, c := range testCases {
		t.Run(c.name, func(t *testing.T) {
			got := isGamePossible(c.g, c.constraint)
			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("isGamePossible() missmatch (-want +got):\n%s", diff)
			}
		})
	}
}
func TestParseGame(t *testing.T) {
	type testCase struct {
		name string
		line string
		want Game
		err  error
	}

	testCases := []testCase{
		{
			name: "simple parse",
			line: "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
			want: Game{
				Game: 1,
				Plays: []Play{
					{
						Red:   4,
						Blue:  3,
						Green: 0,
					},
					{
						Red:   1,
						Blue:  6,
						Green: 2,
					},
					{
						Red:   0,
						Blue:  0,
						Green: 2,
					},
				},
			},
		},
		{
			name: "only one game",
			line: "Game 80: 38 blue, 4 red",
			want: Game{
				Game: 80,
				Plays: []Play{
					{
						Red:   4,
						Blue:  38,
						Green: 0,
					},
				},
			},
		},
	}

	for _, c := range testCases {
		t.Run(c.name, func(t *testing.T) {
			got, err := parseGame(c.line)
			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("parseGame() missmatch (-want +got):\n%s", diff)
			}

			if diff := cmp.Diff(c.err, err); diff != "" {
				t.Errorf("parseGame() missmatch (-want +got):\n%s", diff)
			}
		})
	}
}
