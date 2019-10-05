{-# LANGUAGE TemplateHaskell #-}
import CurryHT(genCurries)

$(genCurries 62)