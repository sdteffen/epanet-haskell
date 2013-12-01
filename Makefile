%.o : %.c
	gcc -c -o $@ $<

c_objs = epanet.o input1.o input2.o input3.o rules.o output.o report.o \
	inpfile.o hydraul.o smatrix.o quality.o mempool.o hash.o

epanet_haskell_example: epanet_haskell_example.hs Epanet.hs $(c_objs)
	ghc -o epanet_haskell_example $(c_objs) Epanet.hs epanet_haskell_example.hs

default: epanet_haskell_example
