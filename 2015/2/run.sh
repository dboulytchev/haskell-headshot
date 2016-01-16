function check () {
  if [ $? -ne 0 ] ; then exit 255 ; fi
}

ghc ImpTest.hs
check

./ImpTest
check
