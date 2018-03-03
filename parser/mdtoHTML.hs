import System.IO
import MdHTMLParse

main = do
  contents <- fmap readMdBlock . readFile $ "test_input.md"
  writeFile "test_output.html" . writeHTMLBlock $ contents

