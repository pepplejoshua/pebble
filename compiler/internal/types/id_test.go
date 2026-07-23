package types

import "testing"

func TestTypeIDValidity(t *testing.T) {
	if TypeID(0).IsValid() {
		t.Fatal("zero TypeID must be invalid")
	}
	if !TypeID(1).IsValid() {
		t.Fatal("nonzero TypeID must be valid")
	}
}
