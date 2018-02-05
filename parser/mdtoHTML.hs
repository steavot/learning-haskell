import System.IO
import MdHTMLParse

main = do
    contents <- readFile "test_input.md"
    writeFile "test_output.html" (writeHTMLBlock . readMdBlock $ contents)

