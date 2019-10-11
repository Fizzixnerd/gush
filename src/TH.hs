module TH (text) where

import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Shakespeare.Text (st)

text :: QuasiQuoter
text = st

