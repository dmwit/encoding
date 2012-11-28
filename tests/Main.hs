import Control.Monad
import Test.HUnit
import Test.Tests

hunitTests =
	[ ("utf8Tests", utf8Tests)
	, ("utf16Tests", utf16Tests)
	, ("punycodeTests", punycodeTests)
	, ("isoTests", isoTests)
	, ("jisTests", jisTests)
	, ("gb18030Tests", gb18030Tests)
	]

main = do
	identityTests
	forM_ hunitTests $ \(name, test) -> do
		putStrLn $ "running " ++ name
		runTestTT test >>= print
