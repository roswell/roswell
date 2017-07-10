roslispdir = $(sysconfdir)/$(PACKAGE)
roslisp_DATA = helper.el \
	init.lisp \
	util.lisp \
	util-swank.lisp \
	locations.lisp \
	patch-quicklisp.lisp \
	system.lisp \
	\
	build.ros \
	build-ros.lisp build-asd.lisp \
	\
	config.ros \
	emacs.ros \
	fmt.ros \
	get.ros \
	git.ros \
	hg.ros \
	run.ros \
	setup.ros \
	shell.ros \
	version.ros \
	wait.ros \
	\
	delete.ros \
	delete-dump.lisp delete-git.lisp delete-asdf.lisp \
	\
	dump.ros \
	util-dump.lisp \
	dump-ccl.lisp dump-clisp.lisp \
	dump-cmucl.lisp dump-ecl.lisp dump-sbcl.lisp \
	\
	help.ros \
	help-install.lisp help-options.lisp help-run.lisp \
	\
	init.ros \
	init-default.lisp \
	\
	install.ros \
	util-install.lisp util-install-quicklisp.lisp \
	install+7zip.lisp install-abcl-bin.lisp install-allegro.lisp \
	install-ccl-bin.lisp install-clasp.lisp install-clisp.lisp \
	install-cmu-bin.lisp install-ecl.lisp install-quicklisp.lisp \
	install-sbcl-bin.lisp install-sbcl.lisp install-slime.lisp \
	install+externals-clasp.lisp install+ffcall.lisp \
	install+msys2.lisp install+sigsegv.lisp install-asdf.lisp \
	\
	update.ros \
	update-git.lisp update-hg.lisp update-quicklisp.lisp \
	\
	list.ros \
	list-dump.lisp list-git.lisp list-installed.lisp list-versions.lisp \
	list-asdf.lisp \
	\
	template.ros \
	util-template.lisp \
	\
	use.ros \
	util-use.lisp \
	use-asdf.lisp \
	\
	which.ros

../src/gend.h: FORCE
	printf '#define ROS_COMPILE_ENVIRONMENT "%s"\n' "`$(CC) --version|head -n 1`" > $@.tmp
	(printf "#define ROS_REVISION \"" && ((which git>/dev/null&&[ -e ../.git ]&& \
	(git log -n 1 --oneline|cut -d' ' -f1| tr -d '\n'| tr -d '\r'))||printf "") && printf "\"\n") >> $@.tmp
	printf '#ifdef HAVE_WINDOWS_H\n'  >> $@.tmp
	printf '#define LISP_PATH "%s"' "`cd $(roslispdir); pwd -W 2>/dev/null`" >> $@.tmp
	printf '\n#else\n'  >> $@.tmp
	printf '#define LISP_PATH "%s"' "$(roslispdir)" >> $@.tmp
	printf '\n#endif\n'  >> $@.tmp
	cmp -s $@.tmp $@||cp $@.tmp $@
	rm -f $@.tmp

FORCE:
.PHONY: FORCE
