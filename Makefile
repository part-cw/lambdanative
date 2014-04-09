all:
	@ ./make.sh all

clean:
	@ ./make.sh clean

scrub:
	@ ./make.sh scrub

install:
	@ ./make.sh install

install-tool:
	@ ./make.sh install-tool

package:
	@ ./make.sh package

executable:
	@ ./make.sh executable 

payload:
	@ ./make.sh payload

info:
	@ ./make.sh info

gcc:
	@ ./make.sh gcc
