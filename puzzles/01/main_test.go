package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/patrickpichler/advent-of-code/pkgs/util"
)

func TestExtractNumbers(t *testing.T) {
	type TestCase struct {
		input string
		want  []byte
	}

	for _, c := range []TestCase{
		{
			input: "one",
			want:  []byte{1},
		},
		{
			input: "eightwo",
			want:  []byte{8, 2},
		},
		{
			input: "eightwo3",
			want:  []byte{8, 2, 3},
		},
		{
			input: "asdf;aks1a;hkb;seh9",
			want:  []byte{1, 9},
		},
		{
			input: "asdfbweeightassaegasetgaonasdasegseneone",
			want:  []byte{8, 1},
		},
	} {
		t.Run(c.input, func(t *testing.T) {
			got := extractNumbers(c.input)

			if diff := cmp.Diff(c.want, got); diff != "" {
				t.Errorf("extractNumbers() missmatch (-want +got):\n%s", diff)
			}
		})
	}
}

func BenchmarkPart1(b *testing.B) {
	data, err := util.HandleInput("input")

	if err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		Part1(data)
	}
}

func BenchmarkPart2(b *testing.B) {
	data, err := util.HandleInput("input")

	if err != nil {
		b.Fatal(err)
	}

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		Part2(data)
	}
}
