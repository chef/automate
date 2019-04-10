DOT=dot
PLANTUML=plantuml

diagrams = layers.png context-map.png package.png deployment-single-local.png \
           deployment-single-ssh.png deployment-multi-ssh.png deployment-ha-ssh.png \
           context-map-2017-08-31.png gateway-sequence.png deployment-config-sequence.png \
           runtime-config-sequence.png

all: $(diagrams)

.PHONY: clean

clean:
	rm *.png

%.png: %.iuml
	$(PLANTUML) $<

%.png: %.dot
	$(DOT) -Tpng -o$@ $<

