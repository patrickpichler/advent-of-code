package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestExtractNumber(t *testing.T) {
	type result struct {
		Found bool
		Num   int
		Start int
		End   int
	}

	type testCase struct {
		name string
		line string
		col  int
		want result
	}

	testCases := []testCase{
		{
			name: "number at start",
			line: "123...456",
			col:  1,
			want: result{
				Found: true,
				Num:   123,
				Start: 0,
				End:   2,
			},
		},
		{
			name: "number at start, col at end of number",
			line: "123...456",
			col:  2,
			want: result{
				Found: true,
				Num:   123,
				Start: 0,
				End:   2,
			},
		},
		{
			name: "number at end",
			line: "123...456",
			col:  7,
			want: result{
				Found: true,
				Num:   456,
				Start: 6,
				End:   8,
			},
		},
		{
			name: "number at end, col at end",
			line: "123...456",
			col:  8,
			want: result{
				Found: true,
				Num:   456,
				Start: 6,
				End:   8,
			},
		},
		{
			name: "number in middle",
			line: "123.2..456",
			col:  4,
			want: result{
				Found: true,
				Num:   2,
				Start: 4,
				End:   4,
			},
		},
    {
			name: "number not found",
			line: "123.2..456",
			col:  6,
			want: result{
				Found: false,
				Num:   0,
				Start: 0,
				End:   0,
			},
		},
	}

	for _, c := range testCases {
		t.Run(c.name, func(t *testing.T) {
			gotFound, gotNum, gotStart, gotEnd := extractNumber(c.col, []rune(c.line))

			got := result{gotFound, gotNum, gotStart, gotEnd}

			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("extractNumber() missmatch (-want +got):\n%s", diff)
			}
		})
	}
}
