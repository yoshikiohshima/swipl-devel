################################################################
# Build jpl.jar
################################################################

.SUFFIXES: .java .class

JAVAC=$(JAVA_HOME)\bin\javac -sourcepath .;fli
JAR=$(JAVA_HOME)\bin\jar
JPL=..\..\..\jpl.jar

CLS=	Atom.java \
	Compound.java \
	Float.java \
	Integer.java \
	JBoolean.java \
	JPLException.java \
	JPL.java \
	JRef.java \
	JVoid.java \
	PrologException.java \
	Query.java \
	Term.java \
	Util.java \
	Variable.java \
	Version.java

FLI=	fli\atom_t.java \
	fli\BooleanHolder.java \
	fli\DoubleHolder.java \
	fli\engine_t.java \
	fli\fid_t.java \
	fli\functor_t.java \
	fli\IntHolder.java \
	fli\LongHolder.java \
	fli\module_t.java \
	fli\ObjectHolder.java \
	fli\PointerHolder.java \
	fli\predicate_t.java \
	fli\Prolog.java \
	fli\qid_t.java \
	fli\StringHolder.java \
	fli\term_t.java

JAVA=$(FLI) $(CLS)
CLASSES=$(JAVA:.java=.class)

all:	$(JPL)

$(JPL):	$(JAVA)
	$(JAVAC) $(JAVA)
	$(JAR) cf $(JPL) $(CLASSES)

clean::
	if exist *.class del *.class
	if exist *~ del *~

distclean:	clean
	if exist $(JPL) del $(JPL)
