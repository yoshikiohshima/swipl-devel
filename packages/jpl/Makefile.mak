################################################################
# Build the SWI-Prolog tabling package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk

PKGDLL=jpl

EXDIR=		$(PKGDOC)\examples\jpl
EXPL=		$(EXDIR)\prolog
EXPLS=		jpl_colour_choose_demo.pl \
		jpl_jlist_demo.pl \
		jpl_midi_demo.pl \
		jpl_table_demo.pl \
		jpl_text_entry_demo.pl \
		jpl_versions_demo.pl \
		README.TXT

CFLAGS =	$(CFLAGS) \
		-I"$(JAVA_HOME)\include" \
		-I"$(JAVA_HOME)\include\win32"
LIBS =		$(LIBS) $(JAVA_HOME)\lib\jvm.lib

OBJ=		src\c\jpl.obj

all:		checkenv $(PKGDLL).dll jar

jar::
		chdir src\java & $(MAKE)

checkenv::
		@if not exist "$(JAVA_HOME)\lib\jvm.lib" \
			echo FATAL ERROR: No JAVA_HOME defined? && exit 1

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll "$(BINDIR)"
ilib::
		copy jpl.pl "$(PLBASE)\library"
		copy jpl.jar "$(PLBASE)\lib"
		$(MAKEINDEX)

html-install::	install-examples

xpce-install::

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		if not exist "$(EXPL)/$(NULL)" $(MKDIR) "$(EXPL)"
		cd examples\prolog & \
			@for %f in ($(EXPLS)) do @copy %f "$(EXPL)"

uninstall::
		del "$(PLBASE)\bin\$(PKGDLL).dll"
		del "$(PLBASE)\library\jpl.pl"
		del "$(PLBASE)\lib\jpl.jar"
		$(MAKEINDEX)

clean::
		if exist $(OBJ) del $(OBJ)
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.pdb *.ilk 2>nul

