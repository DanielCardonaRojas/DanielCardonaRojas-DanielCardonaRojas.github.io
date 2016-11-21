--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import 		     Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "static/*/*" $ do
	route idRoute
	compile copyFileCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
	    >>= loadAndApplyTemplate "templates/page.html" siteCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

{-
    -- TODO: use tags, postCtx a function from Tags to Context
    -- see: https://github.com/CBMM/CBMM.github.io/blob/hakyll/site.hs
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "feedcontent"
            renderRss decarojFeedConfig feedCtx posts
-}

--------------------------------------------------------------------------------
decarojFeedConfig :: FeedConfiguration
decarojFeedConfig = FeedConfiguration
  { feedTitle = "Daniel Cardona"
  , feedDescription = "My own discoveries about software"
  , feedAuthorName = "Daniel Cardona"
  , feedAuthorEmail = "d.cardona.rojas@gmail.com"
  , feedRoot = "https://danielcardonarojas.github.io"
  }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx 

siteCtx :: Context String
siteCtx = 
    constField "baseurl" "https://danielcardonarojas.github.io" `mappend` 
    constField "site_description" "My Programming Blog" `mappend`
    constField "instagram_username" "" `mappend`
    constField "twitter_username" "CocolateCardona" `mappend`
    constField "github_username" "DanielCardonaRojas" `mappend`
    constField "google_username" "Daniel Cardona Rojas" `mappend`
    constField "blog_author" "Daniel Cardona Rojas" `mappend`
    constField "blog_author_initials" "D.C" `mappend`
    defaultContext
