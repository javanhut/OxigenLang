package main

import (
	"os"
	"path/filepath"
	"testing"
)

func labels(items []CompletionItem) map[string]bool {
	set := make(map[string]bool, len(items))
	for _, it := range items {
		set[it.Label] = true
	}
	return set
}

func wantLabels(t *testing.T, items []CompletionItem, want ...string) {
	t.Helper()
	got := labels(items)
	for _, w := range want {
		if !got[w] {
			t.Errorf("missing completion %q (have %d items)", w, len(items))
		}
	}
}

func TestGeneralCompletionsIncludeDocSymbolsAndConcurrency(t *testing.T) {
	src := "struct Person {\n    name <str>\n}\n\nfun greet() {}\n\nWIDTH := 80\n\nmain {\n    here\n}"
	pos := posAt(t, src, "her")
	items := getCompletions(src, pos, "", "")
	// User-declared symbols appear as you write them...
	wantLabels(t, items, "Person", "greet", "WIDTH")
	// ...alongside the new concurrency keywords/builtin.
	wantLabels(t, items, "diverge", "converge", "within", "cancel")
}

func TestInstanceDotCompletesFieldsAndMethods(t *testing.T) {
	src := "struct Person {\n    name <str>\n    age <int>\n}\nPerson includes {\n    fun greet() { name }\n}\nmain {\n    p := Person(\"Alice\", 30)\n    p.\n}"
	pos := posAt(t, src, "    p.")
	items := getCompletions(src, pos, "", "")
	wantLabels(t, items, "name", "age", "greet")
}

func TestSelfDotInsideIncludesBlock(t *testing.T) {
	src := "struct Counter {\n    count <int>\n}\nCounter includes {\n    fun bump() {\n        self.\n    }\n}"
	pos := posAt(t, src, "self.")
	items := getCompletions(src, pos, "", "")
	wantLabels(t, items, "count", "bump")
}

func TestInstanceDotWalksInheritance(t *testing.T) {
	src := "struct Animal {\n    name <str>\n}\nAnimal includes {\n    fun speak() { name }\n}\nstruct Dog(Animal) {\n    breed <str>\n}\nmain {\n    d := Dog(\"Rex\", \"Lab\")\n    d.\n}"
	pos := posAt(t, src, "    d.")
	items := getCompletions(src, pos, "", "")
	wantLabels(t, items, "breed", "name", "speak")
}

func TestTypeAnnotationOffersUserStructs(t *testing.T) {
	src := "struct Account {\n    balance <int>\n}\nmain {\n    a <\n}"
	pos := posAt(t, src, "a <")
	items := getCompletions(src, pos, "", "")
	wantLabels(t, items, "Account", "int", "str")
}

func TestLocalModuleDotCompletesImportedFunctions(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "helpers.oxi"), []byte("fun pad_left(s, width) {}\nfun trim_all(s) {}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	mainPath := filepath.Join(dir, "main.oxi")
	src := "introduce .helpers\nmain {\n    helpers.\n}"
	pos := posAt(t, src, "helpers.")
	items := getCompletions(src, pos, "file://"+mainPath, "")
	wantLabels(t, items, "pad_left", "trim_all")
}
