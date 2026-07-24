package infer

// InferID identifies one mutable inference class inside a Session.
type InferID uint32

// ConstraintID identifies one constraint in deterministic insertion order.
type ConstraintID uint32

// OriginID identifies one copied source origin inside a Session.
type OriginID uint32

// TemplateID identifies one immutable Program-owned type template.
type TemplateID uint32

// SlotID identifies one ordered checker-owned publication in a Session.
// Its fields remain private so an ID cannot be forged from its ordinal.
type SlotID struct {
	owner   *sessionToken
	ordinal uint32
}

// ChoiceRef is the session-owned capability required to guard a solved slot.
type ChoiceRef struct {
	owner        *sessionToken
	constraint   ConstraintID
	alternatives uint32
}

func (id InferID) IsValid() bool      { return id != 0 }
func (id ConstraintID) IsValid() bool { return id != 0 }
func (id OriginID) IsValid() bool     { return id != 0 }
func (id TemplateID) IsValid() bool   { return id != 0 }
