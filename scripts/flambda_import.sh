set -e
ocamltmp=$(mktemp -d -t ocaml-XXXXXXXXXX)
git clone https://github.com/ocaml-flambda/ocaml  --single-branch --branch flambda2.0-stable $ocamltmp

subtree=ocaml/

patchtmp=$(mktemp -d -t patches-XXXXXXXXX)
mkdir -p $patchtmp
rev=e7fa1c0f00bfc2db6f64b6605f6189d95d65682b

pushd $ocamltmp
# First squash all commits between $rev and HEAD
git checkout -b foobar
git reset --soft $rev^
git commit -am "Flambda 2"
patchfile=$(git format-patch -p --src-prefix=a/ocaml/ --dst-prefix=b/ocaml/ HEAD^ -o $patchtmp)
git checkout flambda2.0-stable
# Then checkout to the revision right before $rev
git checkout $rev^
popd

pushd $patchtmp
# Alter the patch to make it look like as if the file were moved
find . -name "*.patch" -exec sed -i "s/^rename from /rename from ocaml\//g" {} \;
find . -name "*.patch" -exec sed -i "s/^rename to /rename to ocaml\//g" {} \;
find . -name "*.patch" -exec sed -i "s/ocaml\/middle_end\//middle_end\//g" {} \;
find . -name "*.patch" -exec sed -i "s/ocaml\/asmcomp\//backend\//g" {} \;
find . -name "*.patch" -exec sed -i "s/ocaml\/toplevel\//native_toplevel\//g" {} \;
find . -name "*.patch" -exec sed -i "s/ocaml\/driver\//driver\//g" {} \;
find . -name "*.patch" -exec sed -i "s/ocaml\/file_formats\//file_formats\//g" {} \; 
popd

echo $patchtmp
# Create a new branch empty branch
git branch -D __branch_to_delete
git checkout -b __branch_to_delete
git reset --hard $(git rev-list HEAD --reverse | head -n 1)
# Force copy ocaml to be the one before flambda was introduced
# and move folders as expected by the flambda-backend layout.
cp -r $ocamltmp ocaml
rm -rdf ocaml/.git
mv ocaml/asmcomp backend
mv ocaml/driver driver
mv ocaml/file_formats file_formats
mv ocaml/middle_end middle_end
mv ocaml/toplevel native_toplevel
git add ocaml
git add backend driver file_formats middle_end native_toplevel
git commit -am "Import ocaml base code"
importedocamlroot=$(git rev-parse HEAD)

echo $patchtmp
# Apply the patch on this empty branch
git am $patchfile

echo $importedocamlroot
# Rebase the changes to flambda
#git checkout flambda
git rebase --onto flambda $importedocamlroot HEAD

rm -rdf $ocamltmp
rm -rdf $patchtmp