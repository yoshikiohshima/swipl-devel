//tabstop=4

import jpl.Query;				// empirically, we need this, but I don't know why...
import jpl.fli.Prolog;
import jpl.*;

public class Test2
{
	public static void
	main( java.lang.String argv[] )
	{
		
		/*
		Prolog.set_default_init_args(		// in case Prolog isn't started
			new String[] {
				"libpl.dll",
				"-L8m",
				"-G16m",
				"-f", "library(jpl)"
			}
		);
		 */

		System.out.print( "calling Prolog to call Java to call Prolog...\n" );

		System.out.println( "factorial(10) = " + jpl.Test.fac(10));
	}
	
}
