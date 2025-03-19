module Main where

import MarkdownRealCode (compileSuperMarkdown)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "compileSuperMarkdown" $ do
    it "compiles a source reference to a link and code block" $ do
      -- let input = "See this example:\n[>src:eg/auth.ts]\nIt works great."
      result <-
        compileSuperMarkdown $
          unlines
            [ "See this example:",
              "[>src:eg/auth.ts]",
              "It works great."
            ]
      result
        `shouldBe` unlines
          [ "See this example:",
            "[eg/auth.ts](https://github.com/your-username/your-project/tree/main/eg/auth.ts)",
            "",
            "```ts",
            "export function auth() {",
            "\treturn true;",
            "}\n",
            "```",
            "It works great."
          ]
