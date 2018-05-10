
#####################################################################################################
COURSE=cs130s
ASGN=04
NAME=nano
STACK=stack
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
	tar -zcvf ../$(ASGN)-$(NAME).tgz --exclude .stack-work --exclude .git ../$(ASGN)-$(NAME)
	mv ../$(ASGN)-$(NAME).tgz . 
	turnin -c $(COURSE) -p $(ASGN) ./$(ASGN)-$(NAME).tgz  

