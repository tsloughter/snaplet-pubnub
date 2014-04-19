{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

{-|

PubNub snaplet.

-}

module Snap.Snaplet.PubNub
    ( PubNub
    , initPubNub
    , publish
    , subscribe )

where

import Control.Lens
import Control.Monad.State
import Data.Aeson

import Snap.Snaplet
import Network.Pubnub.Types
import Control.Concurrent.Async

import qualified Data.Text as T
import qualified Network.Pubnub as PN

------------------------------------------------------------------------------
-- | Snaplet's state data type
data PubNub = PubNub
    { _config :: PN
    }

makeLenses ''PubNub
  
publish :: (MonadIO m, MonadState app m) =>
                 Simple Lens app (Snaplet PubNub) -> T.Text -> T.Text -> m (Maybe PublishResponse)
publish snaplet channel msg = do
  pn <- gets $ view (snaplet . snapletValue . config)
  r <- liftIO $ PN.publish pn channel msg
  return r

subscribe :: (MonadIO m, MonadState app m, FromJSON a) =>
                 Simple Lens app (Snaplet PubNub) -> SubscribeOptions a -> m (Async ())
subscribe snaplet subOpts = do
  pn <- gets $ view (snaplet . snapletValue . config)
  r <- liftIO $ PN.subscribe pn subOpts
  return r

initPubNub :: T.Text -> T.Text -> T.Text -> Bool -> SnapletInit b PubNub
initPubNub chan sk pk useSSL =
    makeSnaplet "snaplet-pubnub" "PubNub snaplet." Nothing $ do
      let pn = defaultPN {channels=[chan], sub_key=sk, pub_key=pk, ssl=useSSL }
      return $ PubNub pn
