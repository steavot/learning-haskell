-- run like: cat test_input.md | runhaskell ParserCmdLineIO.hs
import MdHTMLParse

main = interact (writeHTMLBlock . readMdBlock)

