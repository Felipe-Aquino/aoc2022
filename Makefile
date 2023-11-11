all: help

OC=ocamlopt

content := open Utils\n
content += \n
content += let part1 filename = ()\n
content += \n
content += let part2 filename = ()\n
content += \n
content += (* Calling the solutions *)\n
content += \n
content += let () =\n
content += \tlet raw_args = Args.read () in\n
content += \tlet parsed = Args.parse raw_args in\n
content += \tlet filename =\n
content += \t\tmatch parsed.file with\n
content += \t\t| None -> \"./inputs/example.txt\"\n
content += \t\t| Some name -> name\n
content += \tin\n
content += \t\tif parsed.part == 1 then\n
content += \t\t\tpart1 filename\n
content += \t\telse if parsed.part == 2 then\n
content += \t\t\tpart2 filename\n

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

day4: utils.ml day4.ml
	$(OC) -o $@ $^

day5: utils.ml day5.ml
	$(OC) -o $@ $^

day6: utils.ml day6.ml
	$(OC) -o $@ $^

day7: utils.ml day7.ml
	$(OC) -o $@ $^

day8: utils.ml day8.ml
	$(OC) -o $@ $^

day9: utils.ml day9.ml
	$(OC) -o $@ $^

day10: utils.ml day10.ml
	$(OC) -o $@ $^

day11: utils.ml day11.ml
	$(OC) -o $@ $^

day12: utils.ml day12.ml
	$(OC) -o $@ $^

day13: utils.ml day13.ml
	$(OC) -o $@ $^

day14: utils.ml day14.ml
	$(OC) -o $@ $^

day15: utils.ml day15.ml
	$(OC) -o $@ $^

#<--

%.ml: rep = $(patsubst %.ml,%,$@): utils.ml $(patsubst %.ml,%,$@).ml\n\t$$(OC) -o $$@ $$^\n\n\#<--

%.ml:
	@sed -i 's/^#<--/${rep}/' Makefile
	@echo "$(content)" | sed 's/^ //g;s/\t/  /g' - >> $@

clean:
	rm *.o *.cmx *.cmi
	ls | grep -e 'day[0-9]\+$$' - | xargs rm
