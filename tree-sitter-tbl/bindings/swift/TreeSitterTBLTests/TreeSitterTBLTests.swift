import XCTest
import SwiftTreeSitter
import TreeSitterTbl

final class TreeSitterTblTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_tbl())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Task Based Language grammar")
    }
}
