package source

import "testing"

func TestFileSetStripsBOMAndTracksPositions(t *testing.T) {
	set := NewFileSet()
	id, err := set.Add("sample.peb", []byte("\xef\xbb\xbfα\n\tvalue"))
	if err != nil {
		t.Fatal(err)
	}
	file, ok := set.File(id)
	if !ok {
		t.Fatal("source file was not stored")
	}
	if got := string(file.Text()); got != "α\n\tvalue" {
		t.Fatalf("text = %q", got)
	}
	if got := file.Position(2); got != (Position{Line: 1, Column: 2}) {
		t.Fatalf("position after alpha = %+v", got)
	}
	if got := file.Position(4); got != (Position{Line: 2, Column: 5}) {
		t.Fatalf("position after tab = %+v", got)
	}
}

func TestFileSetRejectsInvalidUTF8(t *testing.T) {
	set := NewFileSet()
	if _, err := set.Add("bad.peb", []byte{0xff}); err == nil {
		t.Fatal("expected invalid UTF-8 error")
	}
}
