package tree_sitter_TBL_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-TBL"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_TBL.Language())
	if language == nil {
		t.Errorf("Error loading Tbl grammar")
	}
}
