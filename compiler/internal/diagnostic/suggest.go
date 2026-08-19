package diagnostic

// EditDistance returns the Levenshtein edit distance between two strings: the
// minimum number of single-character insertions, deletions, and substitutions
// needed to turn a into b. It is computed with the standard two-row dynamic
// program, using bytes as the unit of distance. It is intended for diagnostic
// suggestion heuristics, not for hot paths.
func EditDistance(a, b string) int {
	// Ensure b is the shorter string so the row stays small.
	if len(a) < len(b) {
		a, b = b, a
	}
	if len(b) == 0 {
		return len(a)
	}
	previous := make([]int, len(b)+1)
	current := make([]int, len(b)+1)
	for j := 0; j <= len(b); j++ {
		previous[j] = j
	}
	for i := 1; i <= len(a); i++ {
		current[0] = i
		for j := 1; j <= len(b); j++ {
			cost := 0
			if a[i-1] != b[j-1] {
				cost = 1
			}
			best := current[j-1] + 1
			if deletion := previous[j] + 1; deletion < best {
				best = deletion
			}
			if substitution := previous[j-1] + cost; substitution < best {
				best = substitution
			}
			current[j] = best
		}
		previous, current = current, previous
	}
	return previous[len(b)]
}

// Suggest returns the candidate closest to target within an edit-distance
// threshold, or false when no candidate is close enough. The threshold mirrors
// rustc's heuristic: a candidate is accepted when its distance is small
// relative to the target's length (max(1, len(target)/3)). Ties resolve to
// the lexicographically smallest candidate so the result is deterministic
// regardless of candidate order.
func Suggest(target string, candidates []string) (string, bool) {
	if target == "" {
		return "", false
	}
	threshold := 1
	if limit := len(target) / 3; limit > threshold {
		threshold = limit
	}
	best := ""
	bestDistance := int(^uint(0) >> 1)
	for _, candidate := range candidates {
		if candidate == "" || candidate == target {
			continue
		}
		lengthDelta := len(target) - len(candidate)
		if lengthDelta < 0 {
			lengthDelta = -lengthDelta
		}
		// Edit distance is never below the absolute length difference, so any
		// candidate whose lengths differ by more than the threshold cannot win.
		if lengthDelta > threshold {
			continue
		}
		distance := EditDistance(target, candidate)
		if distance < bestDistance || (distance == bestDistance && candidate < best) {
			bestDistance = distance
			best = candidate
		}
	}
	if best == "" || bestDistance > threshold {
		return "", false
	}
	return best, true
}