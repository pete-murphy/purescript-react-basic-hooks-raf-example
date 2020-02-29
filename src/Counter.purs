module Counter where

import Prelude
import Data.DateTime.Instant (unInstant)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import React.Basic.DOM as R
import React.Basic.Hooks
  ( Hook
  , ReactComponent
  , UseEffect
  , UseRef
  , coerceHook
  , component
  , readRefMaybe
  , useEffect
  , useRef
  , useState
  , writeRef
  , (/\)
  )
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.Window (RequestAnimationFrameId, cancelAnimationFrame, requestAnimationFrame)

mkCounter :: Effect (ReactComponent {})
mkCounter = do
  component "Counter" \_ -> React.do
    count /\ setCount <- useState 0
    useAnimationFrame \deltaTime ->
      setCount \prevCount -> (Int.round (Int.toNumber prevCount + deltaTime)) `mod` 10000
    pure
      $ R.div_ [ R.text (show count) ]

newtype UseAnimationFrame hooks
  = UseAnimationFrame (UseEffect Unit (UseRef (Nullable Number) (UseRef (Nullable RequestAnimationFrameId) hooks)))

derive instance ntUseAnimationFrame :: Newtype (UseAnimationFrame hooks) _

useAnimationFrame :: (Number -> Effect Unit) -> Hook UseAnimationFrame Unit
useAnimationFrame callback =
  coerceHook React.do
    requestRef <- useRef null
    previousTimeRef <- useRef null
    let
      animate = do
        w <- window
        Milliseconds time <- unInstant <$> now
        previousTime <- readRefMaybe previousTimeRef
        case previousTime of
          Just pt -> callback (time - pt)
          Nothing -> mempty
        writeRef previousTimeRef (notNull time)
        a <- requestAnimationFrame animate w
        writeRef requestRef (notNull a)
    useEffect unit do
      w <- window
      a <- requestAnimationFrame animate w
      writeRef requestRef (notNull a)
      r <- readRefMaybe requestRef
      pure (maybe mempty (\frameId -> cancelAnimationFrame frameId w) r)
