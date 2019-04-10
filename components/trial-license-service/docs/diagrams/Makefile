DOT = dot
PLANTUML = plantuml

diagrams = $(patsubst %.iuml,%.png,$(wildcard *.iuml))

all: $(diagrams)

.PHONY: clean

clean:
	rm *.png

%.png: %.iuml
	$(PLANTUML) $<

%.png: %.dot
	$(DOT) -Tpng -o$@ $<
