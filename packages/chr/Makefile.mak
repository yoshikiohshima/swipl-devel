################################################################
# Install CHR stuff for the MS-Windows built
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
LIBDIR=$(PLBASE)\library
CHR=$(LIBDIR)\chr

all::		

!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		@if not exist "$(CHR)\$(NULL)" $(MKDIR) "$(CHR)"
		copy chr_runtime.pl "$(CHR)"
		copy chr_op.pl "$(CHR)"
		copy chr_translate.pl "$(CHR)"
		copy chr_debug.pl "$(CHR)"
		copy chr_swi.pl "$(LIBDIR)\chr.pl"
		copy README "$(LIBDIR)\README.TXT"
		$(MAKEINDEX)
!ENDIF

xpce-install::
html-install::

uninstall::
		del "$(CHR)\chr_runtime.pl"
		del "$(CHR)\chr_op.pl"
		del "$(CHR)\chr_translate.pl"
		del "$(CHR)\chr_debug.pl"
		del "$(CHR)\README.TXT"
		del "$(LIBDIR)\chr.pl"
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


