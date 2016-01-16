function check () {
  if [ $? -ne 0 ] ; then exit 255 ; fi
}


ghc EqClassesTest.hs
check

./EqClassesTest
check

