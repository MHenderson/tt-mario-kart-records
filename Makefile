all: png

png: plot/mario-kart.png

plot/mario-kart.png:
	Rscript -e "targets::tar_make()"

clean:
	rm plot/mario-kart.png
