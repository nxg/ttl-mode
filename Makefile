# To make a 'release', the repository owner should:
#
#   * increment the RELEASE variable below
#   * commit
#   * make
#   * upload the resulting ttl-mode.el to https://bitbucket.org/nxg/ttl-mode/downloads/
#   * tag the repository appropriately

RELEASE=0.2

ttl-mode.el: ttl-mode.el.in
	rm -f ttl-mode.el
	V=`hg parent --template "ttl-mode $(RELEASE) ({node|short}, of {date|rfc3339date}; see https://bitbucket.org/nxg/ttl-mode)"`; \
	  sed "s|@VERSION@|$$V|" ttl-mode.el.in >ttl-mode.el
