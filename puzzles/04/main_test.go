package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestParseLine(t *testing.T) {
	type testCase struct {
		name string
		line string
		want Card
	}

	for _, c := range []testCase{
		{
			name: "simple",
			line: "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
			want: Card{
				Num:            1,
				WinningNumbers: []int{41, 48, 83, 86, 17},
				Numbers:        []int{83, 86, 6, 31, 17, 9, 48, 53},
			},
		},
	} {
		t.Run(c.name, func(t *testing.T) {
			got := parseLine(c.line)
			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("parseLine() missmatch (-want +got):\n%s", diff)
			}
		})
	}

}
