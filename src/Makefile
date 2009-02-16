
#
# see also Emakefile
#

SRC = erlview
BEAM = 

E_SRC=$(foreach file, $(SRC), ./$(file).erl)
E_BEAM=$(foreach file, $(SRC), ../ebin/$(file).beam)

ERLMAKE=/usr/local/bin/erl -make
ERLEVAL=/usr/local/bin/erl -eval
MAKE=/usr/bin/make




all : $(E_SRC)
	$(ERLMAKE)
	$(MAKE) doc


doc : $(E_SRC)
	$(ERLEVAL) 'edoc:run([],[erlview.erl], [{new,true},{source_path, [".", "../../couchdb/src/couchdb"]}, {dir, "../doc"}]) , init:stop().'
#% end