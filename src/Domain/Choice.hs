module Domain.Choice where

data Choice = Yes | No

isYes :: Choice -> Bool
isYes Yes = True
isYes No  = False
