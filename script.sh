make 
x=$?

if [ $x == 0 ]
then
  cd 1
  make check
  x=$?
  cd ..
  exit $x
else
  exit $x
fi

