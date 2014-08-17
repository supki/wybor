-- | Collection of utilities to make @wybor@ customization palatable
--
-- Those are mostly thin wrappers over things in "System.Console.ANSI" from @ansi-terminal@
module Ansi
  ( reset
  , bold
  , regular
  , underlining
  , swap
  , unswap
  , fgcolor
  , bgcolor
  , Ansi.Underlining(..)
  , Ansi.ColorIntensity(..)
  , Ansi.Color(..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Console.ANSI as Ansi


-- | Sets all attributes off
reset :: Text
reset = sgr Ansi.Reset

-- | Set bold font style
bold :: Text
bold = sgr (Ansi.SetConsoleIntensity Ansi.BoldIntensity)

-- | Set regular font style
regular :: Text
regular = sgr (Ansi.SetConsoleIntensity Ansi.NormalIntensity)

-- | Set underlining style
underlining :: Ansi.Underlining -> Text
underlining = sgr . Ansi.SetUnderlining

-- | Swap foreground and background colors
swap :: Text
swap = sgr (Ansi.SetSwapForegroundBackground True)

-- | Unswap foreground and background colors
unswap :: Text
unswap = sgr (Ansi.SetSwapForegroundBackground False)

-- | Set foreground color
fgcolor :: Ansi.ColorIntensity -> Ansi.Color -> Text
fgcolor i c = sgr (Ansi.SetColor Ansi.Foreground i c)

-- | Set background color
bgcolor :: Ansi.ColorIntensity -> Ansi.Color -> Text
bgcolor i c = sgr (Ansi.SetColor Ansi.Background i c)

sgr :: Ansi.SGR -> Text
sgr = Text.pack . Ansi.setSGRCode . return
