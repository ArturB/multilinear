./test.sh && 
echo -e "\nBuilding doc..." && 
stack haddock --coverage && 
echo -e "\nUploading to Hackage..." && 
stack sdist && 
stack upload .
