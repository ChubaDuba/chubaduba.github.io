
{-# LANGUAGE Arrows             #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where


import           Hakyll
import           Data.Monoid     ((<>))
import           Prelude         hiding (id)
import           System.Exit     (ExitCode)
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Data.Text as T
import qualified System.Process  as Process
import qualified Text.Pandoc     as Pandoc


main :: IO ()
main = hakyll $ do
    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
            "favicon.ico" .||. "files/**" .||. "fonts/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                    listField "posts" (postCtx tags) (return posts) <>
                    defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" ctx
            >>= loadAndApplyTemplate "templates/content.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
    
    -- Create RSS feed as well
    version "rss" $ do
        route   $ setExtension "xml"
        compile $ loadAllSnapshots pattern "content"
            >>= fmap (take 10) . recentFirst
            >>= renderRss (feedConfiguration title) feedCtx

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx
    
    -- Resume as HTML
    match "resume.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/resume.html"  defaultContext
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Photo's
    match "photos/*.md" $ compile getResourceBody

    -- Photo Blog
    photoBlog <- buildPaginateWith
        (\ids -> sortRecentFirst ids >>= return . paginateEvery 3)
        "photos/*.md"
        (\n -> if n == 1
            then "photos.html"
            else fromCapture "photos/*.html" (show n))
    paginateRules photoBlog $ \pageNum pattern -> do
        route idRoute
        compile $ do
            photos <- recentFirst =<< loadAll pattern
            let paginateCtx = paginateContext photoBlog pageNum
            let ctx         =
                    constField "title" "Photos"                        <>
                    listField "photos"
                        (photographCtx <> paginateCtx) (return photos) <>
                    paginateCtx                                        <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/photo.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    where
    pages =
        [ "contact.markdown"
        , "links.markdown"
        ]

--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "ChubaDuba - " ++ title
    , feedDescription = "ChubaDuba - Блог"
    , feedAuthorName  = "ChubaDuba"
    , feedAuthorEmail = "achubakov@gmail.com"
    , feedRoot        = "https://chubaduba.github.io"
    }


photographCtx :: Context String
photographCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , metadataField
    ]
