
# .SECONDEXPANSION:

pop = $(filter-out $(firstword $(1)), $(1))

# $(info a b c)
# $(info $(strip a b c))
# $(info $(firstword $(strip a b c)))
# $(info $(call pop,$(strip a b c)))
# $(info a b c)

c_dump = ros dump                       $(call pop,$(1)) executable aaa.ros -o c-$(firstword $(1)).bin
u_dump = ros dump --disable-compression $(call pop,$(1)) executable aaa.ros -o u-$(firstword $(1)).bin

targets = baseline docstring destroy package packages except combined

.PHONY: $(targets) all clean

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
combined:        u-combined.bin  c-combined.bin


%-baseline.bin  : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, baseline)
	./$@
%-docstring.bin : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, docstring --remove-docstrings)
	./$@
%-destroy.bin   : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, destroy   --destroy-packages-sbcl)
	./$@
%-package.bin  : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, package  --delete-package trivia)
	./$@
%-packages.bin  : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, packages  --delete-all-packages)
	./$@
%-except.bin    : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, except --delete-packages-except trivia)
	./$@
%-combined.bin    : aaa.ros
	$(call $(firstword $(subst -, ,$@))_dump, combined --remove-docstrings --delete-all-packages --destroy-packages-sbcl)
	./$@


clean:
	rm -f *.bin
