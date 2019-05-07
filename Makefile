
#####################################################################################################
COURSE=cs130w
ASGN=04
NAME=nano
STACK=stack --allow-different-user
# BUILD_OPTS=--ghc-options -O0 
#####################################################################################################

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

test: clean
	$(STACK) test 

bin:
	$(STACK) build

clean: 
	$(STACK) clean

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c lib/

turnin: 
	# rm -rf .stack-work
	rm -rf ./$(ASGN)-$(NAME).tgz
	tar -zcvf ../$(ASGN)-$(NAME).tgz --exclude .stack-work --exclude .git .
	mv ../$(ASGN)-$(NAME).tgz . 

