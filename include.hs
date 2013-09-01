-- includes.hs
import Text.Pandoc

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = do
   contents <- getContents
   let mdOld = readMarkdown def contents
   mdNew <- bottomUpM doInclude mdOld
   putStrLn $ writeMarkdown def mdNew
