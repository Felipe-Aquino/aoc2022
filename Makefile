all: help

OC=ocamlopt

content := let part1 filename = ();;\n
content += \n
content += let part2 filename = ();;\n
content += \n
content += let raw_args = Utils.Args.read () in\n
content += let parsed = Utils.Args.parse raw_args in\n
content += let filename =\n
content += \tmatch parsed.file with\n
content += \t| None -> \"./inputs/example.txt\"\n
content += \t| Some name -> name\n
content += in\n
content += \tif parsed.part == 1 then\n
content += \t\tpart1 filename\n
content += \telse if parsed.part == 2 then\n
content += \t\tpart2 filename\n
content += ;;\n

help:
	@echo "";
	@echo "  make day<number>   Compiles solution for a day. Example: make day1";
	@echo "                      - run example: ./day1 part:1";
	@echo "";
	@echo "  make clean         Clears the generated ocaml files";
	@echo "";

day1: utils.ml day1.ml
	$(OC) -o $@ $^

day2: utils.ml day2.ml
	$(OC) -o $@ $^

day3: utils.ml day3.ml
	$(OC) -o $@ $^

#<--

%.ml: rep = $(patsubst %.ml,%,$@): utils.ml $(patsubst %.ml,%,$@).ml\n\t$$(OC) -o $$@ $$^\n\n\#<--

%.ml:
	@sed -i 's/^#<--/${rep}/' Makefile
	@echo "$(content)" | sed 's/^ //g;s/\t/  /g' - >> $@

clean:
	rm *.o *.cmx *.cmi
	ls | grep -e 'day[0-9]\+$$' - | xargs rm
