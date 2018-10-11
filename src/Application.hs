module Application
    ( ApplicationT(..)
    , Definition(..)
    , main
    , run
    )
where

import Domain.Application (ApplicationT(..))
import Domain.MainLogic   (main)
import Free.Monolith      (Definition(..), run)
