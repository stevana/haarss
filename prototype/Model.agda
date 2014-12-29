module Model where

open import Data.Product
open import Data.Char
open import Data.String
open import Data.Container.Indexed
open import IO.Primitive

open import Signature

postulate
  Model        : Set
  initialModel : Model

  moveModel          : Dir → Model → Model
  setMode            : Mode → Model → Model
  getPrompt          : Model → Prompt
  addCharToBuffer    : Char → Model → Model
  getBuffer          : Model → String
  setCurrentFeed     : Model → Feed → Model
  getCurrentFeedsUrl : Model → String
  searchModel        : String → Model → Model
  searchNextModel    : String → Model → Model
  addFeedToModel     : String → Model → Model

cmdToResp : ∀ {m} (c : Command Hårss m) → Model → IO (Response Hårss c)
cmdToResp (moveP d)    m = return _
cmdToResp (promptP p)  m = return _
cmdToResp (putCharP c) m = return _
cmdToResp doneP        m = return (getBuffer m , getPrompt m)
cmdToResp fetchP       m = fetchUrl (getCurrentFeedsUrl m)
  where
  postulate
    fetchUrl : String → IO Feed
cmdToResp searchNextP  m = return (getBuffer m)
