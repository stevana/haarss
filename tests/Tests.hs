{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Haarss.Feed.Annotated
import {-@ HTF_TESTS @-} Haarss.Model
import {-@ HTF_TESTS @-} Haarss.Model.Window

main = htfMain htf_importedTests
