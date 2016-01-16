function check () {
  if [ $? -ne 0 ] ; then exit 255 ; fi
}

ghc RelTest.hs
check

./RelTest
check

#ghc EqClassesTest.hs
#check

#./EqClassesTest
#check

ghc ImpTest.hs
#check

#./ImpTest
#check
