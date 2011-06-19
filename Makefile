all:
	./overlay.sh test.tex test2.tex
	./dvi-overlay.sh test.dvi test2.dvi out.dvi
	dvipdfmx out.dvi

DviOverlay: DviOverlay.hs DviAsm.hs
	ghc --make -O2 DviOverlay
	#ghc --make -O2 -prof -auto-all -caf-all -rtsopts -osuf p_o -hisuf p_hi DviOverlay
