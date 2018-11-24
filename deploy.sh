./test.sh && 
echo -e "Building doc...\n" && 
stack haddock --coverage && 
echo -e "Uploading to Hackage...\n" && 
stack sdist && 
stack upload .
