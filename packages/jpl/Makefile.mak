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
CFLAGS =	$(CFLAGS) \
		-I"$(JAVA_HOME)\include" \
		-I"$(JAVA_HOME)\include\win32"
LIBS =		$(LIBS) $(JAVA_HOME)\lib\jvm.lib

OBJ=		src\c\jpl.obj

all:		checkenv $(PKGDLL).dll jar

jar::
		chdir src\java\jpl & $(MAKE)

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

html-install::

xpce-install::

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

