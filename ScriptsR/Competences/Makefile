
all: ../../Output/competences.pdf ../../Output/ensimag_competences_graphe.pdf

../../Output/competences.pdf : competences.R
	R --vanilla --no-save < competences.R

../../Output/ensimag_competences_graphe.pdf: compet13.csv
	perl ./compet_biparti.pl compet13.csv > graph13.dot
	circo -Tpdf graph13.dot > $@
