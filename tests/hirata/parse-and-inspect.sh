
set -eux

folder=$1

rm $1/*.lisp || echo nothing to remove

make -j4 $(echo $1/*.out | sed s/out/lisp/g)

for i in $1/*.out; do
  vim -O ${i%.out}.lisp $i
done
