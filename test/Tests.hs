module Tests (test) where

import Distribution.TestSuite

test :: String -> Result -> Test
test name result = Test t
  where
    t =
      TestInstance
        { run = return (Finished result),
          name = name,
          tags = [],
          options = [],
          setOption = \_ _ -> Right t
        }
