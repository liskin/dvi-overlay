all:
	./overlay.sh test.tex test2.tex && dviasm test.dvi >test.dasm && dviasm test2.dvi >test2.dasm && ./DviOverlay test.dasm test2.dasm test3.dasm && dviasm test3.dasm >test3.dvi && dvipdfmx test3.dvi && okular test3.pdf

DviOverlay: DviOverlay.hs DviAsm.hs
	ghc --make -O2 DviOverlay
	#ghc --make -O2 -prof -auto-all -caf-all -rtsopts -osuf p_o -hisuf p_hi DviOverlay
