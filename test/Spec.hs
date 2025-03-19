module Main where

import MarkdownRealCode (compileSuperMarkdown)
import Test.Hspec (describe, hspec, it, shouldBe)

-- Mock file reading for testing
mockReadFile :: String -> IO String
mockReadFile "eg/auth.ts" = return "export function auth() {\n  return true;\n}"
mockReadFile _ = return "unknown file"

main :: IO ()
main = hspec $ do
  describe "compileSuperMarkdown" $ do
    it "compiles a source reference to a link and code block" $ do
      let input = "See this example:\n[>src:eg/auth.ts]\nIt works great."
      result <- compileSuperMarkdown input
      result
        `shouldBe` unlines
          [ "See this example:",
            "[eg/auth.ts](https://github.com/your-username/your-project/tree/main/eg/auth.ts)",
            "",
            "```ts",
            "export function auth() {",
            "  return true;",
            "}",
            "```",
            "It works great."
          ]
