function check () {
  if [ $? -ne 0 ] ; then exit 255 ; fi
}

ghc RelTest.hs
check

ghc EqClassesTest.hs
check

./EqClassesTest
check
