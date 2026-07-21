----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.CSS  as CSS
import qualified Miso.Html as H
import qualified Miso.Html.Property as H
#ifdef INTERACTIVE
import           Miso.Reload (liveWithContext)
#endif
----------------------------------------------------------------------------
-- | The app-global, React-style @context@: a single counter shared by every
-- context-aware 'Component'. Whenever it changes (per its 'Eq' instance) every
-- 'Component' with @useContext@ enabled re-renders against the new value.
--
-- Read it via the first argument threaded into 'view'; write it from 'update'
-- with 'modifyContext' \/ 'putContext'.
type Context = Int
----------------------------------------------------------------------------
-- | Entry point. 'startAppWithContext' seeds the global @context@ (here @0@)
-- before the first draw.
main :: IO ()
#ifdef INTERACTIVE
main = liveWithContext defaultEvents (0 :: Context) root
#else
main = startAppWithContext defaultEvents (0 :: Context) root
#endif
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
-- | Top-level component.
--
-- The runtime always makes the root context-aware, so it re-renders on every
-- context change. It owns no model of its own — it just displays the shared
-- context, offers a reset, and mounts the three children.
data RootAction = ResetContext
----------------------------------------------------------------------------
root :: Component Context () () RootAction
root = component () update view
  where
    update ResetContext = putContext 0
    view ctx _ _ =
      H.div_ [ CSS.style_ pageStyle ]
        [ H.h1_ [ CSS.style_ titleStyle ]
            [ "🍜 "
            , H.a_
              [ H.href_ "https://github.com/haskell-miso/context"
              , H.target_ "blank"
              ]
              [ "miso-context" ]
            ]
        , H.p_ [ CSS.style_ subtitleStyle ]
            [ "A single app-global "
            , H.code_ [] [ "context" ]
            , " counter is shared by every component tagged "
            , H.strong_ [] [ "uses context" ]
            , ". Bump it from anywhere and each subscriber re-renders in step."
            ]
        , H.div_ [ CSS.style_ (cardStyle "solid" accentRoot) ]
            [ componentHeader "Root (App root)"
            , stateSection accentRoot
                [ sectionLabel "App-global context"
                , infoRow "context" (ms ctx)
                , buttonRow [ btn accentRoot ResetContext "Reset to 0" ]
                ]
            ]
        , H.div_ [ CSS.style_ rowStyle ]
            [ mountUseContext childA
            , mountUseContext childB
            , mount_ childC
            ]
        ]
----------------------------------------------------------------------------
-- | Child A — uses the context. Bumps it by @+1@.
data AAction = IncA
----------------------------------------------------------------------------
childA :: Component Context () () AAction
childA = component () update view
  where
    update IncA = modifyContext (+ 1)
    view ctx _ _ =
      childCard "solid" accentA "Child A" $
        [ badge accentA "uses context"
        , propsSection accentA
            [ sectionLabel "Shared context"
            , infoRow "context" (ms ctx)
            , buttonRow [ btn accentA IncA "context + 1" ]
            ]
        ]
----------------------------------------------------------------------------
-- | Child B — uses the context. Bumps it by @+10@.
data BAction = IncB
----------------------------------------------------------------------------
childB :: Component Context () () BAction
childB = component () update view
  where
    update IncB = modifyContext (+ 10)
    view ctx _ _ =
      childCard "solid" accentB "Child B" $
        [ badge accentB "uses context"
        , propsSection accentB
            [ sectionLabel "Shared context"
            , infoRow "context" (ms ctx)
            , buttonRow [ btn accentB IncB "context + 10" ]
            ]
        ]
----------------------------------------------------------------------------
-- | Child C — does __not__ use the context (mounted with 'mount_', so
-- @useContext@ stays 'False').
--
-- It still receives the current context in its 'view', but because it opts out
-- of context propagation it only re-renders in response to its own actions.
-- Watch its \"context\" readout go stale while A/B/Root update, then snap to the
-- live value the moment you bump its local counter.
data CAction = BumpLocal
----------------------------------------------------------------------------
childC :: Component Context () Int CAction
childC = component 0 update view
  where
    update BumpLocal = modify (+ 1)
    view ctx _ localCount =
      childCard "dashed" accentC "Child C" $
        [ badge accentC "opts out"
        , propsSection accentC
            [ sectionLabel "Context (stale until I re-render)"
            , infoRow "context" (ms ctx)
            ]
        , stateSection accentC
            [ sectionLabel "Local state"
            , infoRow "my counter" (ms localCount)
            , buttonRow [ btn accentC BumpLocal "bump (forces re-render)" ]
            ]
        ]
----------------------------------------------------------------------------
-- =====================================================================
-- Palette (miso-props style)
-- =====================================================================

accentRoot, accentA, accentB, accentC :: MisoString
accentRoot = "#6c5ce7"   -- purple
accentA    = "#00b894"   -- green
accentB    = "#e17055"   -- orange
accentC    = "#b2bec3"   -- muted gray (opts out)

----------------------------------------------------------------------------
-- =====================================================================
-- Shared view helpers
-- =====================================================================

-- | A titled, colored card used by the child components.
childCard :: MisoString -> MisoString -> MisoString -> [View model action] -> View model action
childCard border color title body =
  H.div_ [ CSS.style_ (cardStyle border color) ]
    (componentHeader title : body)

componentHeader :: MisoString -> View model action
componentHeader label =
  H.div_ [ CSS.style_ headerStyle ] [ text label ]

-- | Small pill next to a component header, tinted with the card's accent.
badge :: MisoString -> MisoString -> View model action
badge color label =
  H.span_
    [ CSS.style_
        [ CSS.display "inline-block"
        , CSS.fontSize "0.7rem"
        , CSS.fontWeight "700"
        , CSS.letterSpacing "0.06em"
        , CSS.padding "2px 8px"
        , CSS.borderRadius "999px"
        , CSS.marginBottom "12px"
        , CSS.color (CSS.hex color)
        , CSS.border ("1px solid " <> color)
        ]
    ]
    [ text label ]

infoRow :: MisoString -> MisoString -> View model action
infoRow label val =
  H.div_
    [ CSS.style_
        [ CSS.display "flex"
        , CSS.gap "6px"
        , CSS.alignItems "center"
        , CSS.marginBottom "4px"
        , CSS.fontSize "0.9rem"
        ]
    ]
    [ H.span_ [ CSS.style_ [ CSS.fontWeight "700" ] ] [ text (label <> ":") ]
    , H.span_ [ CSS.style_ [ CSS.color (CSS.hex "#444") ] ] [ text val ]
    ]

sectionLabel :: MisoString -> View model action
sectionLabel label =
  H.div_
    [ CSS.style_
        [ CSS.fontSize "0.7rem"
        , CSS.fontWeight "700"
        , CSS.color (CSS.hex "#888")
        , CSS.letterSpacing "0.08em"
        , CSS.marginBottom "8px"
        ]
    ]
    [ text label ]

btn :: MisoString -> action -> MisoString -> View model action
btn color action label =
  H.button_ [ H.onClick action, CSS.style_ (btnStyle color) ] [ text label ]

buttonRow :: [View model action] -> View model action
buttonRow children =
  H.div_ [ CSS.style_ [ CSS.display "flex", CSS.gap "8px", CSS.marginTop "10px" ] ]
    children

-- | Dashed inner section keyed to an accent color (props from parent / context).
propsSection :: MisoString -> [View model action] -> View model action
propsSection color children =
  H.div_ [ CSS.style_ (innerSectionStyle color) ] children

-- | Dashed inner section keyed to an accent color (component-owned state).
stateSection :: MisoString -> [View model action] -> View model action
stateSection color children =
  H.div_ [ CSS.style_ (innerSectionStyle color) ] children

----------------------------------------------------------------------------
-- =====================================================================
-- Styles
-- =====================================================================

pageStyle :: [CSS.Style]
pageStyle =
  [ CSS.fontFamily "system-ui, -apple-system, sans-serif"
  , CSS.padding "28px 32px"
  , CSS.maxWidth "960px"
  , CSS.margin "0 auto"
  , CSS.color (CSS.hex "#222")
  ]

titleStyle :: [CSS.Style]
titleStyle =
  [ CSS.fontSize "2rem"
  , CSS.fontWeight "800"
  , CSS.margin "0 0 8px 0"
  ]

subtitleStyle :: [CSS.Style]
subtitleStyle =
  [ CSS.margin "0 0 28px 0"
  , CSS.color (CSS.hex "#555")
  , CSS.lineHeight "1.6"
  ]

rowStyle :: [CSS.Style]
rowStyle =
  [ CSS.display "flex"
  , CSS.gap "18px"
  , CSS.alignItems "flex-start"
  , CSS.flexWrap "wrap"
  , CSS.marginTop "18px"
  ]

cardStyle :: MisoString -> MisoString -> [CSS.Style]
cardStyle border color =
  [ CSS.border ("2px " <> border <> " " <> color)
  , CSS.borderRadius "10px"
  , CSS.padding "18px"
  , CSS.minWidth "220px"
  , CSS.flex "1"
  , CSS.backgroundColor (tint color)
  ]

headerStyle :: [CSS.Style]
headerStyle =
  [ CSS.fontWeight "800"
  , CSS.fontSize "1.05rem"
  , CSS.marginBottom "10px"
  , CSS.color (CSS.hex "#333")
  ]

innerSectionStyle :: MisoString -> [CSS.Style]
innerSectionStyle color =
  [ CSS.padding "14px"
  , CSS.border ("1px dashed " <> color)
  , CSS.borderRadius "8px"
  , CSS.marginBottom "12px"
  , CSS.backgroundColor (tint color)
  ]

btnStyle :: MisoString -> [CSS.Style]
btnStyle color =
  [ CSS.padding "6px 16px"
  , CSS.border "none"
  , CSS.borderRadius "5px"
  , CSS.cursor "pointer"
  , CSS.backgroundColor (CSS.hex color)
  , CSS.color (CSS.hex "#fff")
  , CSS.fontWeight "700"
  , CSS.fontSize "0.95rem"
  ]

-- | A very faint wash of an accent color, used as a card / section background.
tint :: MisoString -> CSS.Color
tint = \case
  c | c == accentRoot -> CSS.rgba 108  92 231 0.05
    | c == accentA    -> CSS.rgba   0 184 148 0.05
    | c == accentB    -> CSS.rgba 225 112  85 0.05
    | otherwise       -> CSS.rgba 178 190 195 0.10
----------------------------------------------------------------------------
