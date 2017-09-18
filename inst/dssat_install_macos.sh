#! /bin/bash
brew install cmake

export dssat_dir=~/DSSAT47

mkdir $dssat_dir

cd $dssat_dir

curl -LJO https://raw.githubusercontent.com/bocinsky/dssat-docker/master/dssat47.tar.gz

tar -zxvf dssat47.tar.gz

cd $dssat_dir/dssat-csm
mkdir build
cd build
cmake ..
make
cp ./bin/dscsm047 $dssat_dir/dscsm047
cp -r ../Data/. $dssat_dir/
cd $dssat_dir
rm ./dssat47.tar.gz
rm -r ./dssat-csm

ln -s "$dssat_dir/dscsm047" /bin/dscsm047

curl -LJO https://raw.githubusercontent.com/bocinsky/dssat-docker/master/DSSATPRO.L47

sed -i '' 's@root/DSSAT47@'"$dssat_dir"'@g' DSSATPRO.L47

cd $dssat_dir/Model_testing/Nitrogen_x_irrigation/
$dssat_dir/dscsm047 A NIIR0001.MZX
sed -i '' 's/\\/\//g' NIIR01MZ.v46
$dssat_dir/dscsm047 B NIIR01MZ.v46
