package tests

import munit.Location
import munit.TestOptions

/**
  * Test for request "textDocument/semanticTokens/full"
  * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
  */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {
  
  test("Sample") {
    for{
    _ <-  initialize(
          s"""/metals.json
            |{"a":{}}
            |/a/src/main/scala/a/Main.scala
            |object Main{def add(a:int):int{a + 1}}
            |""".stripMargin,
            expectError = true
    )
    
    expectedToken:List[Int]=  List(
      //1:deltaLine, 2:deltaStartChar, 3:length, 
      //4:tokenType, 5:tokenModifiers
      1, 7,  4, 1, 0, //Main,Class, No-Modifier
      0, 9,  3, 11, 0, //add, Function, No-Modifier
      0, 4,  1, 6, 0, //a of "a:int",  No-Modifier
      0, 11, 1, 6, 0 //a of "a + 1",  No-Modifier
    )
    _ <- server.didOpen("a/src/main/scala/a/Main.scala")
    _ <- server.assertSemanticHighlight(
          "a/src/main/scala/a/Main.scala",
          expectedToken)

    } yield()
  }



}
