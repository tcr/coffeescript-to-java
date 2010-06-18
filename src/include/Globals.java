public class Globals {
	public static void print(CSPrimitive[] args) {
		for (CSPrimitive arg : args)
			System.out.print(CSUtil.asString(arg));
	}
	
	public static void println(CSPrimitive[] args) {
		for (CSPrimitive arg : args)
			System.out.print(CSUtil.asString(arg));
		System.out.println("");
	}
	
	/*
	 * ECMA standard
	 */
	
	public static CSString encodeURI(CSPrimitive arg) {
		return new CSString("");
	}
	
	public static CSString encodeURIComponent(CSPrimitive arg) {
		return new CSString("");
	}
	
	public static CSString decodeURI() {
		return new CSString("");
	}
	
	public static CSString decodeURIComponent() {
		return new CSString("");
	}
	
	public static CSNumber parseInt(CSPrimitive s, CSPrimitive radix) {
		if (radix.equals(CS.UNDEFINED))
			return new CSNumber(Number.parseInt(asString(s)));
		return new CSNumber(Integer.parseInt(asString(s), asNumber(radix)));
	}
	
	public static CSNumber parseFloat(CSPrimitive s) {
		return new CSNumber((double) Float.parseFloat(s));
	}
	
	public static CSBoolean isNaN(CSPrimitive arg) {
		return Double.isNaN(asNumber(arg)) ? ES.TRUE : ES.FALSE;
	}
	
	public static CSBoolean isFinite() {
		return !Double.isInfinite(asNumber(arg)) ? ES.TRUE : ES.FALSE;
	}
}