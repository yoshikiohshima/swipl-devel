package jpl;

import java.util.Map;

import jpl.fli.Prolog;

public class Test {

	/* jpl.Text.fac(int) complements jpl_test_fac(+int,-int)
	 * 
	 */
	public static int fac(int n) {
		if (n == 1) {
			return 1;
		} else {
			return n * ((Integer) new Query(new Compound("jpl_test_fac", new Term[] { new Integer(n - 1), new Variable("F")})).oneSolution().get("F")).intValue();
		}
	}

	private static void test10() {

		System.err.println("test10:");
		System.err.println("  java_lib_version = " + JPL.version_string());
		System.err.println("  c_lib_version = " + jpl.fli.Prolog.get_c_lib_version());
		System.err.println("  pl_lib_version = " + new Query(new Compound("jpl_pl_lib_version", new Term[] { new Variable("V")})).oneSolution().get("V"));
		System.err.println("  java.version = " + System.getProperty("java.version"));
		System.err.println("  os.name = " + System.getProperty("os.name"));
		System.err.println("  os.arch = " + System.getProperty("os.arch"));
		System.err.println("  os.version = " + System.getProperty("os.version"));
		System.err.println();
	}

	private static void test10a() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("Y")});
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b")});
		Term goal = new Compound("=", new Term[] { lhs, rhs });

		System.err.println("test10a:");
		System.err.println("  named vars: p(X,Y)=p(a,b) is " + new Query(goal).hasSolution() + '\n');
	}

	private static void test10b() {
		Term lhs = new Compound("p", new Term[] { new Variable("X"), new Variable("X")});
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b")});
		Term goal = new Compound("=", new Term[] { lhs, rhs });

		System.err.println("test10b:");
		System.err.println("  shared vars (same name, different Variable): p(X,X)=p(a,b) is " + new Query(goal).hasSolution() + '\n');
	}

	private static void test10c() {
		Variable X = new Variable("X");
		Term lhs = new Compound("p", new Term[] { X, X });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b")});
		Term goal = new Compound("=", new Term[] { lhs, rhs });

		System.err.println("test10c:");
		System.err.println("  shared vars (same Variable): p(X,X)=p(a,b) is " + new Query(goal).hasSolution() + '\n');
	}

	private static void test10d() {
		Term lhs = new Compound("p", new Term[] { new Variable("_"), new Variable("_")});
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b")});
		Term goal = new Compound("=", new Term[] { lhs, rhs });

		System.err.println("test10d:");
		System.err.println("  anon vars (different Variables): p(_,_)=p(a,b) is " + new Query(goal).hasSolution() + '\n');
	}

	private static void test10e() {
		Variable Anon = new Variable("_");
		Term lhs = new Compound("p", new Term[] { Anon, Anon });
		Term rhs = new Compound("p", new Term[] { new Atom("a"), new Atom("b")});
		Term goal = new Compound("=", new Term[] { lhs, rhs });

		System.err.println("test10e:");
		System.err.println("  anon vars (same Variable): p(_,_)=p(a,b) is " + new Query(goal).hasSolution() + '\n');
	}

	private static void test10f() {
		Atom a = new Atom("a");

		System.err.println("test10f:");
		System.err.println("  atom equality (same Atoms): a = a is " + a.equals(a) + '\n');
	}

	private static void test10g() {

		System.err.println("test10g:");
		System.err.println("  atom equality (different Atoms, same names): a = a is " + (new Atom("a")).equals(new Atom("a")) + '\n');
	}

	private static void test10h() {

		System.err.println("test10h:");
		System.err.println("  mutual recursive Java<->Prolog factorial: fac(10) = " + new Query("jpl_test_fac(10,F)").oneSolution().get("F") + '\n');
	}

	private static void test10i() {
		String text = "fred(B,p(A),[A,B,C])";

		System.err.println("test10i:");
		System.err.println("  textToTerm(\"" + text + "\") = " + Util.textToTerm(text) + '\n');
	}

	private static void test10j() {
		Term l2 = Util.termArrayToList(new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e")});
		Query q9 = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2 }));
		Map[] s9s = q9.allSolutions();

		System.err.println("test10j:");
		for (int i = 0; i < s9s.length; i++) {
			System.err.println("  append(Xs,Ys,[a,b,c,d,e]) -> " + Util.toString(s9s[i]));
		}
		System.err.println();
	}

	private static void test10k() {
		String[] args = jpl.fli.Prolog.get_default_init_args();
		String which;
		String s = "";

		System.err.println("test10k:");
		if (args == null) {
			args = jpl.fli.Prolog.get_actual_init_args();
			which = "actual";
		} else {
			which = "default";
		}
		for (int i = 0; i < args.length; i++) {
			s = s + args[i] + " ";
		}
		System.err.println("  " + which + "_init_args = " + s + '\n');
	}

	private static void test10l() {
		Query q5 = new Query(new Compound("length", new Term[] { new Variable("Zs"), new jpl.Integer(5)}));
		Map s5 = q5.oneSolution();

		System.err.println("test10l:");
		System.err.println("  length(Zs,5)");
		System.err.println("  " + Util.toString(s5));
		System.err.println("  Zs = " + (Term) s5.get("Zs"));
		System.err.println();
	}

	private static void test10m() {
		String text = "append(Xs,Ys,[_,_,_,_,_])";
		Query q = new Query(text);
		Map[] ss = q.allSolutions();

		System.err.println("test10m:");
		System.err.println("  all solutions of " + text);
		for (int i = 0; i < ss.length; i++) {
			System.err.println("  " + Util.toString(ss[i]));
		}
		System.err.println();
	}

	private static void test10n() {

		System.err.println("test10n:");

		new Query("write('JRef(null) -> ')").hasSolution();
		new Query(new Compound("display", new Term[] { new JRef(null)})).hasSolution();
		new Query("nl").hasSolution();

		new Query("write('JRef(int[]) -> ')").hasSolution();
		new Query(new Compound("display", new Term[] { new JRef(new int[] {
			})
			})).hasSolution();
		new Query("nl").hasSolution();

		new Query("write('JBoolean(false) -> ')").hasSolution();
		new Query(new Compound("display", new Term[] { new jpl.JBoolean(false)})).hasSolution();
		new Query("nl").hasSolution();

		new Query("write('JBoolean(true) -> ')").hasSolution();
		new Query(new Compound("display", new Term[] { new jpl.JBoolean(true)})).hasSolution();
		new Query("nl").hasSolution();

		new Query("write('JVoid() -> ')").hasSolution();
		new Query(new Compound("display", new Term[] { new jpl.JVoid()})).hasSolution();
		new Query("nl").hasSolution();

		System.err.println();
	}

	private static void test10o() {

		System.err.println("test10o:");
		Term l2b = Util.termArrayToList(new Term[] { new Variable("A"), new Variable("B"), new Variable("C"), new Variable("D"), new Variable("E")});
		Query q9b = new Query(new Compound("append", new Term[] { new Variable("Xs"), new Variable("Ys"), l2b }));
		Map[] s9bs = q9b.allSolutions();
		for (int i = 0; i < s9bs.length; i++) {
			System.err.println("  append(Xs,Ys,[A,B,C,D,E]) -> " + Util.toString(s9bs[i]));
		}
		System.err.println();
	}

	private static void test10p() {

		System.err.println("test10p:");
		try {
			new Query("p(]");
		} catch (PrologException e) {
			System.err.println("p(] ->" + e.toString());
		}
		System.err.println();
	}

	private static void test10q() {

		System.err.println("test10q:");
		System.err.println((new Atom("simple")).toString());
		System.err.println((new Atom("3 3")).toString());
		System.err.println((new Compound("Bad Name", new Term[] { new Atom("3 3")})).toString());
		System.err.println();
	}

	private static void test10r() {
		String text1 = "fred(?,2,?)";
		String text2 = "[first(x,y),A]";
		Term plist = Util.textToTerm(text2);
		Term[] ps = plist.toTermArray();

		System.err.println("test10r:");
		System.err.println("ps=" + ps + ", ps.toTermArray(): ps[0]=" + ps[0] + ", ps[1]=" + ps[1]);
		System.err.println("putParams(" + text1 + "," + text2 + ") -> " + Util.textToTerm(text1).putParams(ps).toString());
		System.err.println();
	}

	private static void test10s() {
		final Query q = new Query("jpl_slow_goal"); // 10 successive sleep(1)

		System.err.println("test10s:");
		Thread t = new Thread(new Runnable() {
			public void run() {
				try {
					System.err.println("q.hasSolution() ... ");
					System.err.println(q.hasSolution() ? "finished" : "failed");
				} catch (Exception e) {
					System.err.println("q.hasSolution() threw " + e);
				}
			}
		});
		t.start(); // call the query in a separate thread

		System.err.println("pausing for 2 secs...");
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			;
		} // wait a coupla seconds for it to get started

		// (new Query("set_prolog_flag(abort_with_exception, true)")).hasSolution();

		System.err.println("calling q.abort()...");
		q.abort();
		System.err.println();
	}

	private static void test10t(){
		final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
		System.err.println("test10t:");
		
		JPL.setDTMMode(true);
		System.out.println(q.oneSolution().toString());	
		JPL.setDTMMode(false);
		System.out.println(q.oneSolution().toString());	
		JPL.setDTMMode(true);
		System.out.println(q.oneSolution().toString());	
		System.err.println();
	}
	
	public static void main(String argv[]) {

		Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "set_prolog_flag(debug_on_error,false)", "-q" });

		test10k();
		test10();
		test10a();
		test10b();
		test10c();
		test10d();
		test10e();
		test10f();
		test10g();
		test10h();
		test10i();
		test10j();
		test10k();
		test10l();
		test10m();
		test10n();
		test10o();
		test10p();
		test10q();
		test10r();
		// test10s();
		test10t();

		String s = new String("" + '\0' + '\377');
		System.err.println("s.length = " + s.length());
		for (int i = 0; i < s.length(); i++) {
			System.err.print((new Integer(s.charAt(i))).toString() + " ");
		}
		System.err.println();

		System.err.println(new Query("atom_codes(A,[127,128,255,0])").oneSolution().toString());
	}
}
