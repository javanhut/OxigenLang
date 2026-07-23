package main

import "testing"

// posAt returns the Position at the first occurrence of marker in src, with the
// cursor placed at the END of marker. Use a unique marker per case.
func posAt(t *testing.T, src, marker string) Position {
	t.Helper()
	idx := indexOf(src, marker)
	if idx < 0 {
		t.Fatalf("marker %q not found in source", marker)
	}
	target := idx + len(marker)
	line, col := 0, 0
	for i := range target {
		if src[i] == '\n' {
			line++
			col = 0
		} else {
			col++
		}
	}
	return Position{Line: uint32(line), Character: uint32(col)}
}

func indexOf(s, sub string) int {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return i
		}
	}
	return -1
}

func TestInStringLiteralText(t *testing.T) {
	cases := []struct {
		name   string
		src    string
		marker string
		want   bool
	}{
		{"plain code", "x := 1 + 2", "1 + ", false},
		{"inside single-line double", `x := "hello`, "hel", true},
		{"after closed single-line", `x := "hi" + `, `"hi" + `, false},
		{"inside single-line single-quote", "x := 'wor", "wor", true},
		{"inside interpolation is code", `x := "a {b`, "{b", false},
		{"text after interpolation close", `x := "a {b} c`, "} c", true},
		{"single-line does not span newline", "x := \"oops\ny := 5", "y := 5", false},
		{"inside triple double across lines", "x := \"\"\"\nline one\nline tw", "line tw", true},
		{"after triple close", "x := \"\"\"hi\"\"\" + ", "\"\"\" + ", false},
		{"inside triple single-quote", "x := '''multi\nline her", "line her", true},
		{"interpolation inside triple is code", "x := \"\"\"\nsum {x +", "{x +", false},
		{"text after triple interpolation", "x := \"\"\"a {b} c", "} c", true},
		{"empty triple then code", "x := \"\"\"\"\"\" + ", "\"\"\" + ", false},
		{"escaped quote stays in string", `x := "a \" b`, `\" b`, true},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			pos := posAt(t, tc.src, tc.marker)
			if got := inStringLiteralText(tc.src, pos); got != tc.want {
				t.Errorf("inStringLiteralText(%q @ %q) = %v, want %v",
					tc.src, tc.marker, got, tc.want)
			}
		})
	}
}

func TestGetCompletionsSuppressedInStringText(t *testing.T) {
	src := "x := \"\"\"\nsome prose here\n\"\"\""
	pos := posAt(t, src, "prose")
	if items := getCompletions(src, pos, "", "", false); len(items) != 0 {
		t.Errorf("expected no completions inside string text, got %d", len(items))
	}

	// Inside an interpolation expression, completions should still be offered.
	codeSrc := "x := \"\"\"\nval {  \n\"\"\""
	codePos := posAt(t, codeSrc, "{  ")
	if items := getCompletions(codeSrc, codePos, "", "", false); len(items) == 0 {
		t.Errorf("expected completions inside interpolation expression, got none")
	}
}

func TestIntroduceContexts(t *testing.T) {
	idx := indexDocument("", "")
	cases := []struct {
		name    string
		src     string
		marker  string
		want    completionContext
		payload string
	}{
		{"bare keyword", "introduce", "introduce", contextAfterIntroduce, ""},
		{"partial module", "introduce o", "introduce o", contextAfterIntroduce, ""},
		{"intro shorthand partial", "intro o", "intro o", contextAfterIntroduce, ""},
		{"selective after from", "introduce {upper} from st", "from st", contextAfterIntroduce, ""},
		{"inside selective braces", "introduce {up} from strings", "introduce {up", contextAfterModuleDot, "strings"},
	}
	for _, c := range cases {
		pos := posAt(t, c.src, c.marker)
		got, payload := detectCompletionContext(c.src, pos, idx, "")
		if got != c.want || payload != c.payload {
			t.Errorf("%s: got (%v, %q), want (%v, %q)", c.name, got, payload, c.want, c.payload)
		}
	}
}
