DOT=dot
PLANTUML=plantuml

diagrams = deployment-class.png deployment-object.png packages.png

all: $(diagrams)

.PHONY: clean

clean:
	rm *.png

%.png: %.iuml
	$(PLANTUML) $<

%.png: %.dot
	$(DOT) -Tpng -o$@ $<

