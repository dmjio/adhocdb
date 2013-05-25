main:
	ghc --make Main.hs -o adb
clean:
	\rm -f *.o adb *.hi .DS_Store
