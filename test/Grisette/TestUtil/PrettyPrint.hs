{-# LANGUAGE CPP #-}

module Grisette.TestUtil.PrettyPrint (renderedAs, compactRenderedAs) where

import qualified Data.Text as T
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine, Unbounded),
    layoutSmart,
  )
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc
  ( Doc,
    LayoutOptions (LayoutOptions),
    PageWidth (AvailablePerLine, Unbounded),
    layoutSmart,
  )
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
#endif

renderedAs :: Doc ann -> T.Text -> IO ()
renderedAs doc expected = do
  let actual = renderStrict $ layoutSmart (LayoutOptions Unbounded) doc
  if actual == expected
    then return ()
    else
      fail $
        "Expected: " ++ T.unpack expected ++ "\nActual: " ++ T.unpack actual

compactRenderedAs :: Doc ann -> T.Text -> IO ()
compactRenderedAs doc expected = do
  let actual =
        renderStrict $ layoutSmart (LayoutOptions (AvailablePerLine 1 1)) doc
  if actual == expected
    then return ()
    else
      fail $
        "Expected: " ++ T.unpack expected ++ "\nActual: " ++ T.unpack actual
