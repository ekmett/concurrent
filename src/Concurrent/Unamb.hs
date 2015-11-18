module Concurrent.Unamb where

import Concurrent.Unamb.Unsafe

unamb_ :: a -> b -> ()
unamb_ x y = unamb (seq x ()) (seq y ())
