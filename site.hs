--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc
import           System.IO.Unsafe

--------------------------------------------------------------------------------
myPandocCompiler = pandocCompilerWithTransform def def {writerHTMLMathMethod=MathJax ""} (unsafeDupablePerformIO . (doInclude "posts/"))
{- need to add mathjax.js link to template. pandoc adds it if given with MathJax ".."
   but hakyll seems to ignore it -}

doInclude :: FilePath -> Pandoc -> IO Pandoc
doInclude incPath = bottomUpM doInc where
    doInc :: Block -> IO Block
    doInc cb@(CodeBlock (id, classes, namevals) contents) =
      case lookup "include" namevals of
           Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile (incPath ++ f)
           Nothing    -> return cb
    doInc x = return x

main :: IO ()
main = hakyll $ do
    create [".nojekyll"] $ do
        route idRoute
        compile (makeItem ("No jekyll\n"::String))

    create ["README.md"] $ do
        route idRoute
        compile (makeItem ("mehultikekar.github.io\n======================\n\nPersonal website\n"::String))

    match ("images/*.jpg" .||. "images/*.png") $ do
        route   idRoute
        compile copyFileCompiler
    
    match ("posts/src/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*.markdown" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/defaultPost.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "contact.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "research.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
