./test.sh && 
echo -e "Building doc...\n" && 
stack haddock && 
echo -e "Uploading to Hackage...\n" && 
stack sdist && 
stack upload . && 
echo -e "\u001b[32mAll done!\u001b[0m"
