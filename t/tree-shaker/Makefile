
# .SECONDEXPANSION:

pop = $(filter-out $(firstword $(1)), $(1))

# $(info a b c)
# $(info $(strip a b c))
# $(info $(firstword $(strip a b c)))
# $(info $(call pop,$(strip a b c)))
# $(info a b c)

c_dump = ros dump                       $(call pop,$(1)) executable aaa.ros -o $(firstword $(1))
u_dump = ros dump --disable-compression $(call pop,$(1)) executable aaa.ros -o $(firstword $(1))

targets = baseline docstring destroy package packages except debug compiler macro cmacro combined

.PHONY: $(targets) all clean test

all: $(targets)
	ls -lh *.bin
# somehow this does not work
# %: u-%.bin c-%.bin
# %.dep: u-%.bin c-%.bin

baseline:	 u-baseline.bin  c-baseline.bin
docstring:	 u-docstring.bin c-docstring.bin
destroy:	 u-destroy.bin   c-destroy.bin
package:	 u-package.bin   c-package.bin
packages:	 u-packages.bin  c-packages.bin
except:          u-except.bin    c-except.bin
debug:	         u-debug.bin     c-debug.bin
compiler:	         u-compiler.bin     c-compiler.bin
macro:	         u-macro.bin     c-macro.bin
cmacro:	         u-cmacro.bin     c-cmacro.bin
combined:        u-combined.bin  c-combined.bin


dump = $(call $(firstword $(subst -, ,$@))_dump, $@ $(1))

%-baseline.bin  : aaa.ros
	$(call dump)
	./$@
%-docstring.bin : aaa.ros
	$(call dump, --remove-docstrings)
	./$@
%-destroy.bin   : aaa.ros
	$(call dump, --destroy-packages-sbcl)
	./$@
%-package.bin  : aaa.ros
	$(call dump, --delete-package trivia)
	./$@
%-packages.bin  : aaa.ros
	$(call dump, --delete-all-packages)
	./$@
%-except.bin    : aaa.ros
	$(call dump, --delete-packages-except trivia)
	./$@
%-debug.bin    : aaa.ros
	$(call dump, --delete-debug-info)
	./$@
%-compiler.bin    : aaa.ros
	$(call dump, --delete-compiler-information-sbcl)
	./$@
%-macro.bin    : aaa.ros
	$(call dump, --delete-macro-definitions)
	./$@
%-cmacro.bin    : aaa.ros
	$(call dump, --delete-compiler-macro-definitions)
	./$@
%-combined.bin    : aaa.ros
	$(call dump, --remove-docstrings --delete-debug-info \
                     --delete-macro-definitions --delete-compiler-macro-definitions \
                     --delete-compiler-information-sbcl \
                     --delete-all-packages --destroy-packages-sbcl)
	./$@

test:
	for bin in *.bin ; do echo "$$bin        $$(./$$bin)" ; done

clean:
	rm -f *.bin
