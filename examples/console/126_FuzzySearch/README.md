# Example 126: Fuzzy Search - Trigram Similarity, Edit Distance, Phonetic, Weighted

## Overview

This example demonstrates a **Fuzzy Search** system that finds approximate matches using multiple algorithms: trigram similarity (n-gram overlap), Levenshtein edit distance, Soundex phonetic matching, and weighted multi-method scoring. It handles typos, misspellings, and phonetic variants across a product catalog.

## Features Demonstrated

### 1. Trigram Generation
- Generates character-level trigrams (3-character sequences)
- Pads strings with spaces for boundary trigrams
- Case-insensitive trigram extraction
- Foundation for similarity comparison

### 2. Trigram Similarity
- Jaccard-like coefficient: common / (total - common)
- Score range: 0.0 (no match) to 1.0 (identical)
- Handles typos: "Keybaord" vs "Keyboard" = 0.385
- Handles misspellings: "Blutooth" vs "Bluetooth" = 0.583

### 3. Edit Distance (Levenshtein)
- Counts minimum insertions, deletions, substitutions
- Normalized score: 1.0 - (distance / max_length)
- Single-char typos: distance=1, score=0.85-0.89
- Completely different words: high distance, low score

### 4. Phonetic Matching (Soundex)
- American Soundex algorithm implementation
- 4-character code (letter + 3 digits)
- Same code for similar-sounding words
- "Headphones" = "Headfones" = "Hedphones" = H315

### 5. Weighted Fuzzy Search
- Combines trigram (0.4), edit distance (0.4), phonetic (0.2)
- Query "blutooth hedphones" finds "Bluetooth Headphones" at 0.80
- Phonetic boost for sound-alike matches
- Configurable weight distribution

### 6. Typo Tolerance
- Handles common typos: swapped letters, missing characters
- "wireles mouse" -> Wireless Mouse (0.87)
- "mecanical keybord" -> Mechanical Keyboard (0.74)
- "pwoer bank" -> Power Bank (0.63)

### 7. Search Ranking
- All products scored against query
- Results sorted by relevance score
- Top-N results returned
- Low-scoring results filtered out

### 8. Partial Word Matching
- Prefix/substring detection boost
- "head" matches "Headphones" with substring bonus
- "wire" matches "Wireless" with substring bonus
- Combined with trigram similarity

### 9. Multi-Field Search
- Searches across: name, category, brand, description
- Field-specific weights: name=0.4, brand=0.3, desc=0.2, cat=0.1
- "soundmax wireless" finds SoundMax brand products
- Cross-field relevance aggregation

### 10. Statistics
- Trigram index coverage per product
- Soundex index entries
- Search results stored
- Index size metrics

## Architecture

```
+------------------+     +------------------+     +------------------+
| Query Input      |     | Matching Engine  |     | Ranked Results   |
+------------------+     +------------------+     +------------------+
| "blutooth"       |---->| Trigram Sim.     |---->| Product + Score  |
| "hedphones"      |     | Edit Distance    |     | Sorted by rank   |
| typos, variants  |     | Soundex Match    |     | Filtered by min  |
+------------------+     | Weighted Combine |     +------------------+
                          +------------------+
                                  |
                  +---------------+---------------+
                  |               |               |
                  v               v               v
          +-------------+  +-------------+  +-------------+
          | Trigrams    |  | Levenshtein |  | Soundex     |
          +-------------+  +-------------+  +-------------+
          | 3-char seqs |  | DP matrix   |  | Phonetic    |
          | Jaccard sim |  | ins/del/sub |  | 4-char code |
          | 0.0 - 1.0   |  | normalized  |  | 0 or 1      |
          +-------------+  +-------------+  +-------------+
```

## Algorithms

### Trigram Similarity
```
Input: "Mouse" -> "  mouse "
Trigrams: ["  m", " mo", "mou", "ous", "use", "se "]
Similarity = |A ∩ B| / |A ∪ B|
```

### Levenshtein Distance
```
Dynamic programming: O(m*n) time and space
Operations: insert (cost 1), delete (cost 1), substitute (cost 1)
Score = 1.0 - (distance / max(len1, len2))
```

### Soundex
```
Keep first letter, map consonants to digits:
B,F,P,V -> 1 | C,G,J,K,Q,S,X,Z -> 2
D,T -> 3 | L -> 4 | M,N -> 5 | R -> 6
Drop adjacent duplicates, pad to 4 chars
```

## Compilation

```bash
cd 126_FuzzySearch
lazbuild FuzzySearch.lpi
./FuzzySearch
```

## Sample Output

```
2. Similarity: "Bluetooth"/"Blutooth"=0.583, "Keyboard"/"Keybaord"=0.385
3. Edit Distance: 1-char typo -> score 0.85-0.89, unrelated words -> 0.125
4. Soundex: Headphones/Headfones/Hedphones all = H315
6. Typo tolerance: all 6 misspelled queries find correct products
9. Multi-field: brand match "SoundMax" boosts wireless audio products
```

## Related Examples

- **127_Autocomplete** - Typeahead suggestions with prefix matching
- **128_SearchAnalytics** - Search behavior tracking and optimization

## Best Practices

1. **Combine methods**: No single algorithm handles all typo types
2. **Weight tuning**: Adjust weights based on your data characteristics
3. **Minimum threshold**: Filter low-score results to reduce noise
4. **Index trigrams**: Pre-compute trigrams for faster similarity lookup
5. **Soundex limits**: Works best for English names, less reliable for technical terms
6. **Edit distance cost**: Consider weighted operations (transpose cheaper than insert)
7. **Partial matching**: Boost substring/prefix matches for better UX
8. **Field weights**: Primary fields (name) should rank higher than secondary (description)
